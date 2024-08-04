import chisel3._
import chisel3.util._

object SB_Struct {
    class sb_t extends Bundle{
        val addr = UInt(32.W)
        val data = UInt(32.W)
        val wstrb = UInt(4.W)
        val uncache = Bool()
    }
}

import SB_Struct._
class SB_IO extends Bundle {
    // for write in ex stage
    val full            = Output(Bool())
    val addr_ex        = Input(UInt(32.W))
    val st_data_ex     = Input(UInt(32.W))
    val mem_type_ex    = Input(UInt(5.W))
    val uncache_ex     = Input(Bool())

    // for commit in wb stage
    val is_store_num_cmt = Input(UInt(2.W))
    val st_cmt_valid     = Output(Bool())
    val dcache_miss      = Input(Bool())
    val st_addr_cmt      = Output(UInt(32.W))
    val st_data_cmt      = Output(UInt(32.W))
    val st_wlen_cmt      = Output(UInt(2.W))
    val is_uncache_cmt   = Output(Bool())
    val flush            = Input(Bool())

    // for read in ex stage
    val ld_data_mem      = Output(UInt(32.W))
    val ld_hit           = Output(Vec(4, Bool()))
    val em_stall         = Input(Bool()) 
}

class SB(n: Int) extends Module {
    val io = IO(new SB_IO)
    val sb = RegInit(VecInit.fill(n)(0.U.asTypeOf(new sb_t)))

    // Store Buffer pointers
    val head = RegInit(0.U(log2Ceil(n).W))
    val rear = RegInit(0.U(log2Ceil(n).W))
    val size = RegInit(0.U((log2Ceil(n)+1).W))

    // ctrl signals
    val clear = RegInit(false.B)
    val full = size === n.U || clear
    val empty = size === 0.U

    val waiting_cmt     = RegInit(0.U((log2Ceil(n)+1).W))
    val d_wvalid = waiting_cmt.orR
    waiting_cmt := waiting_cmt + io.is_store_num_cmt - (io.st_cmt_valid && !io.dcache_miss)
    io.st_cmt_valid := d_wvalid
    io.full := full

    val offset = PriorityEncoder(sb(head).wstrb)
    io.st_addr_cmt := sb(head).addr + offset
    io.st_data_cmt := sb(head).data >> (offset ## 0.U(3.W))
    io.st_wlen_cmt := OHToUInt(PopCount(sb(head).wstrb))
    io.is_uncache_cmt := sb(head).uncache

    // update buffer size
    when(io.flush){
        clear := true.B
    }.elsewhen(waiting_cmt === 0.U){
        clear := false.B
    }
    val st_valid = io.mem_type_ex(4) && !full
    size := Mux(clear && !waiting_cmt.orR, 0.U, size + st_valid - (io.st_cmt_valid && !io.dcache_miss))

    // update buffer
    head := head + ((io.st_cmt_valid && !io.dcache_miss))
    rear := Mux(clear && !waiting_cmt.orR, head + ((io.st_cmt_valid && !io.dcache_miss)), rear + io.mem_type_ex(4))

    when(clear){
        var start = head + waiting_cmt
        val clr_num = size - waiting_cmt
        for(i <- 0 until n){
            when(i.U < clr_num){
                sb(start(log2Ceil(n)-1, 0)).wstrb := 0.U
            }
            start = start + 1.U
        }
    }.elsewhen(!clear && io.mem_type_ex(4)){
        sb(rear).addr := io.addr_ex(31, 2) ## 0.U(2.W)
        sb(rear).data := (io.st_data_ex << (io.addr_ex(1, 0) ## 0.U(3.W)))(31, 0)
        sb(rear).uncache := io.uncache_ex
        sb(rear).wstrb := ((UIntToOH(UIntToOH(io.mem_type_ex(1, 0))) - 1.U) << io.addr_ex(1, 0))(3, 0)
    }

    // load logic
    val ld_addr = io.addr_ex
    val ld_mask = (UIntToOH(UIntToOH(io.mem_type_ex(1, 0))) - 1.U)(3, 0)
    val sb_order = VecInit.tabulate(n)(i => sb(rear-1.U-i.U))
    val sb_order_reg = ShiftRegister(sb_order, 1, !io.em_stall)
    val ld_data = Wire(Vec(4, UInt(8.W)))
    val ld_hit_mask = ShiftRegister(Mux(io.mem_type_ex(4), 0xf.U, (15.U << UIntToOH(io.mem_type_ex(1, 0)))(3, 0)), 1, !io.em_stall)

    val ld_hit_temp = VecInit.tabulate(n)(i => !(sb_order(i).addr(31, 2) ^ ld_addr(31, 2)))
    for(i <- 0 until 4){
        val addr = ld_addr(31, 2) ## (ld_addr(1, 0) + i.U(2.W))(1, 0)
        val hit = VecInit.tabulate(n)(j => ld_hit_temp(j) && sb_order(j).wstrb(addr(1, 0)))
        val bit_hit = hit.asUInt.orR && ld_mask(i)
        val hit_index = ShiftRegister(PriorityEncoderOH(hit.asUInt), 1, !io.em_stall)
        val hit_byte = Mux1H(hit_index, sb_order_reg.map(_.data)) >> (ShiftRegister(addr(1, 0), 1, !io.em_stall) ## 0.U(3.W))
        ld_data(i) := Mux(ShiftRegister(bit_hit, 1, !io.em_stall), hit_byte, 0.U)
        io.ld_hit(i) := ld_hit_mask(i) | ShiftRegister(bit_hit, 1, !io.em_stall)
    }

    io.ld_data_mem := ld_data.asUInt
}

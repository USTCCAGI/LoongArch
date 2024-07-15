import chisel3._
import chisel3.util._
import Inst_Pack._
import CPU_Config._

class Fetch_Queue_IO extends Bundle{
    val insts_pack          = Input(Vec(2, new inst_pack_PD_t))

    val next_ready          = Input(Bool())
    val insts_valid_decode  = Output(Vec(2, Bool()))
    val insts_pack_id       = Output(Vec(2, new inst_pack_PD_t))
    
    val full                = Output(Bool())
    val flush               = Input(Bool())
}

class Fetch_Queue extends Module{
    val io = IO(new Fetch_Queue_IO)

    /* config */
    val ROW_WIDTH = FQ_NUM / 2       /* 8 / 2 */
    // 创建一个 ROW_WIDTH 长度的一维向量，其中每个元素初始化为类型为 inst_pack_PD_t 的零值
    val initial_row = VecInit.fill(ROW_WIDTH)(0.U.asTypeOf(new inst_pack_PD_t))
    // 创建一个长度为2的二维向量，其中每个元素都是一个 ROW_WIDTH 长度的一维向量
    val initial_queue = VecInit.fill(2)(initial_row)
    // 创建一个带有初始值的寄存器，初始值是上面定义的二维向量
    val queue = RegInit(initial_queue)

    def rotate_left1(x: UInt): UInt = {
        val n = x.getWidth
        x(n-2, 0) ## x(n-1)
    }
    def rotate_left2(x: UInt): UInt = {
        val n = x.getWidth
        x(n-3, 0) ## x(n-1, n-2)
    }

    val head = RegInit(1.U(ROW_WIDTH.W))
    val tail = RegInit(1.U(FQ_NUM.W))
    val tail_odd = VecInit.tabulate(ROW_WIDTH)(i => tail(2*i+1)).asUInt 
    val tail_even = VecInit.tabulate(ROW_WIDTH)(i => tail(2*i)).asUInt
    val rear = tail_odd | tail_even

    val full = (head & rotate_left1(rear)).orR
    val empty = (head & rear).orR

    // Enqueue
    io.full := full

    val tail_bits = VecInit.tabulate(FQ_NUM)(i => tail(i) & io.insts_pack(0).inst_valid)
    val rotated_tail_bits = VecInit.tabulate(FQ_NUM)(i => rotate_left1(tail)(i) & io.insts_pack(1).inst_valid)
    val write_mask = VecInit.tabulate(FQ_NUM)(i => tail_bits(i) | rotated_tail_bits(i)).asUInt

    val write_mask_even = VecInit.tabulate(ROW_WIDTH)(i => write_mask(2*i)).asUInt
    val write_mask_odd = VecInit.tabulate(ROW_WIDTH)(i => write_mask(2*i+1)).asUInt
    
    val write_data_even = Mux(tail_even.orR, io.insts_pack(0), io.insts_pack(1))
    val write_data_odd = Mux(!tail_even.orR, io.insts_pack(0), io.insts_pack(1))

    // even queue write
    for(i <- 0 until ROW_WIDTH){
        when(!full && write_mask_even(i)){
            queue(0)(i) := write_data_even
        }
    }
    // odd queue write
    for(i <- 0 until ROW_WIDTH){
        when(!full && write_mask_odd(i)){
            queue(1)(i) := write_data_odd
        }
    }

    // Dequeue
    for(i <- 0 until 2){
        io.insts_pack_id(i) := Mux1H(head, queue(i))
        io.insts_valid_decode(i) := !empty
    }
    // update ptrs
    when(!full){
        tail := Mux(io.insts_pack(0).inst_valid, Mux(io.insts_pack(1).inst_valid, rotate_left2(tail), rotate_left1(tail)), tail)
    }
    when(io.next_ready && !empty){
        head := rotate_left1(head)
    }
    when(io.flush){
        head := 1.U
        tail := 1.U
    }


}

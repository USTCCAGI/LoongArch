import chisel3._
import chisel3.util._
import Inst_Pack._
import CPU_Config._

class Fetch_Queue_IO extends Bundle{
    val insts_pack            = Input(Vec(2, new inst_pack_PD_t))       // a package that contains two instructions
    val next_ready            = Input(Bool())                           // next stage is ready
    val flush                 = Input(Bool())                           // flush the queue

    val insts_valid_decode    = Output(Vec(2, Bool()))                  //whether the instruction is valid  
    val insts_pack_id         = Output(Vec(2, new inst_pack_PD_t))      // the instruction package that is ready to be sent to the next stage
    val full                  = Output(Bool())                          // the queue is full
}

class Fetch_Queue extends Module{
    val io = IO(new Fetch_Queue_IO)

    val ROW_WIDTH = FQ_NUM / 2

    // two-row queue
    val initial_row = VecInit.fill(ROW_WIDTH)(0.U.asTypeOf(new inst_pack_PD_t))
    val initial_queue = VecInit.fill(2)(initial_row)
    val queue = RegInit(initial_queue)

    def rotate_left_1(x: UInt): UInt = {
        val n = x.getWidth
        Cat(x(n-2, 0), x(n-1))
    }
    def rotate_left_2(x: UInt): UInt = {
        val n = x.getWidth
        Cat(x(n-3, 0), x(n-2, n-1))
    }

    val mask = RegInit(1.U(FQ_NUM.W))
    val mask_0 = VecInit.tabulate(ROW_WIDTH)(i => mask(2*i)).asUInt
    val mask_1 = VecInit.tabulate(ROW_WIDTH)(i => mask(2*i+1)).asUInt

    val head = RegInit(1.U(ROW_WIDTH.W))
    val rear = mask_0 | mask_1

    val full = (head === rotate_left_1(rear))
    val empty = (head === rear)

    io.full := full

    val mask_bits = VecInit.tabulate(FQ_NUM)(i => mask(i) & io.insts_pack(0).inst_valid)
    val rotated_mask_bits = VecInit.tabulate(FQ_NUM)(i => rotate_left_1(mask)(i) & io.insts_pack(1).inst_valid)
    val enqueue_mask = mask_bits.asUInt | rotated_mask_bits.asUInt
    val enqueue_mask_0 = VecInit.tabulate(ROW_WIDTH)(i => enqueue_mask(2*i)).asUInt
    val enqueue_data_0 = Mux(mask_0.orR, io.insts_pack(0), io.insts_pack(1))
    val enqueue_mask_1 = VecInit.tabulate(ROW_WIDTH)(i => enqueue_mask(2*i+1)).asUInt
    val enqueue_data_1 = Mux(!mask_0.orR, io.insts_pack(0), io.insts_pack(1))

    for(i <- 0 until ROW_WIDTH){
        when(!full && enqueue_mask_0(i)){
            queue(0)(i) := enqueue_data_0
        }
        when(!full && enqueue_mask_1(i)){
            queue(1)(i) := enqueue_data_1
        }
    }

    for(i <- 0 until 2){
        io.insts_valid_decode(i) := !empty
        io.insts_pack_id(i) := Mux1H(head, queue(i))
    }

    when(!full && io.insts_pack(0).inst_valid){
        when(io.insts_pack(1).inst_valid){
            mask := rotate_left_2(mask)
        }.elsewhen(!full && io.insts_pack(0).inst_valid){
            mask := rotate_left_1(mask)
        }
    }

    when(io.next_ready && !empty){
        head := rotate_left_1(head)
    }

    when(io.flush){
        head := 1.U
        mask := 1.U
    }

}
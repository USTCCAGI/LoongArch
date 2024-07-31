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
    val queue = RegInit(VecInit.fill(2)(initial_row))

    def rotate_left(x: UInt, n: Int): UInt = {
        val width = x.getWidth
        Cat(x(width - n - 1, 0), x(width - 1, width - n))
    }

    val mask = RegInit(1.U(FQ_NUM.W))
    val mask_0 = VecInit.tabulate(ROW_WIDTH)(i => mask(2*i)).asUInt
    val mask_1 = VecInit.tabulate(ROW_WIDTH)(i => mask(2*i+1)).asUInt

    val head = RegInit(1.U(ROW_WIDTH.W))
    val rear = mask_0 | mask_1

    val full = (head === rotate_left(rear, 1))
    val empty = (head === rear)

    io.full := full

    //Enqueue
    val insts_valid = VecInit(io.insts_pack.map(_.inst_valid))
    val mask_bits = mask & Fill(FQ_NUM, insts_valid(0))
    val rotated_mask_bits = rotate_left(mask, 1) & Fill(FQ_NUM, insts_valid(1))
    val enqueue_mask = mask_bits | rotated_mask_bits
    
    val enqueue_mask_0 = (0 until ROW_WIDTH).map(i => enqueue_mask(2 * i))
    val enqueue_mask_1 = (0 until ROW_WIDTH).map(i => enqueue_mask(2 * i + 1))

    for(i <- 0 until ROW_WIDTH){
        when(!full && enqueue_mask_0(i)){
            queue(0)(i) := io.insts_pack(!mask_0.orR)
        }
        when(!full && enqueue_mask_1(i)){
            queue(1)(i) := io.insts_pack(mask_0.orR)
        }
    }

    //Dequeue

    for(i <- 0 until 2){
        io.insts_valid_decode(i) := !empty
        io.insts_pack_id(i) := Mux1H(head, queue(i))
    }

    //Update head and mask
    when(!full && io.insts_pack(0).inst_valid){
        mask := Mux(io.insts_pack(1).inst_valid, rotate_left(mask, 2), rotate_left(mask, 1))
    }

    when(io.next_ready && !empty){
        head := rotate_left(head, 1)
    }

    when(io.flush){
        head := 1.U
        mask := 1.U
    }

}
import chisel3._
import chisel3.util._
import CPU_Config._

class Free_List(n: Int) extends Module {
  val io = IO(new Bundle {
    val rd_valid = Input(Vec(2, Bool()))
    val rename_en = Input(Vec(2, Bool()))
    val alloc_preg = Output(Vec(2, UInt(log2Ceil(n).W)))
    val empty = Output(Bool())

    val commit_en = Input(Vec(2, Bool()))
    val commit_pprd_valid = Input(Vec(2, Bool()))
    val commit_pprd = Input(Vec(2, UInt(log2Ceil(n).W)))

    val predict_fail = Input(Bool())
    val head_arch = Input(UInt(log2Ceil(n).W))
  })

  def shift_add1(x: UInt): UInt = x(x.getWidth - 2, 0) ## x(x.getWidth - 1)

  val free_list = RegInit(VecInit.tabulate(n)(i => (i + 1).U(log2Ceil(n).W)))
  val head = RegInit(1.U(n.W))
  
  // Initialize tail with a vector where only the last element is 1
  val tail_init = VecInit.tabulate(n)(i => (i.U === (n-1).U)).asUInt
  val tail = RegInit(tail_init)

  // Create empty check signals
  val head_empty = VecInit(head, shift_add1(head))
  io.empty := head_empty.map(_ & tail).reduce(_ | _)

  // Allocate new registers
  val head_idx = Wire(Vec(2, UInt(n.W)))
  var head_now = head
  io.alloc_preg := VecInit.fill(2)(0.U(log2Ceil(n).W))
  for (i <- 0 until 2) {
    head_idx(i) := head_now
    head_now = Mux(io.rd_valid(i) && io.rename_en(i) && !io.empty, shift_add1(head_now), head_now)
    io.alloc_preg(i) := Mux1H(head_idx(i), free_list)
  }
  head := head_now

  // Commit old registers
  val tail_idx = Wire(Vec(2, UInt(n.W)))
  var tail_now = tail
  for (i <- 0 until 2) {
    tail_idx(i) := tail_now
    tail_now = Mux(io.commit_en(i) && io.commit_pprd_valid(i), shift_add1(tail_now), tail_now)
    when(io.commit_en(i) && io.commit_pprd_valid(i)) {
      free_list(OHToUInt(tail_idx(i))) := io.commit_pprd(i)
    }
  }
  tail := tail_now

  // Handle reset
  when(reset.asBool) {
    tail := tail_init
  }

  // Handle flush
  when(io.predict_fail) {
    head := UIntToOH(io.head_arch)
  }
}

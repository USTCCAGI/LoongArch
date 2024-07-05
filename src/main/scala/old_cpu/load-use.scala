import chisel3._
import chisel3.util._

class LoadUse extends Module {
  val io = IO(new Bundle {
    val clk_in = Input(Clock())
    val rst = Input(Bool())
    val RS1 = Input(UInt(5.W))
    val RS2 = Input(UInt(5.W))
    val Rd = Input(UInt(5.W))
    val mem_we = Input(Bool())
    val eFlush = Output(Bool())
    val fstall = Output(Bool())
    val dstall = Output(Bool())
  })

  val fstall_reg = RegInit(false.B)
  val dstall_reg = RegInit(true.B)
  val eFlush_reg = RegInit(true.B)

  when(!io.rst) {
    fstall_reg := false.B
    dstall_reg := true.B
    eFlush_reg := true.B
  } .otherwise {
    when((io.RS1 === io.Rd || io.RS2 === io.Rd) && io.mem_we) {
      fstall_reg := true.B
      dstall_reg := false.B
      eFlush_reg := false.B
    } .otherwise {
      fstall_reg := false.B
      dstall_reg := true.B
      eFlush_reg := true.B
    }
  }

  io.eFlush := eFlush_reg
  io.fstall := fstall_reg
  io.dstall := dstall_reg
}

object LoadUse extends App {
  (new chisel3.stage.ChiselStage).emitVerilog(new LoadUse)
}

import chisel3._

class PC extends Module {
  val io = IO(new Bundle {
    val npc = Input(UInt(32.W))
    val rst = Input(Bool())
    val run = Input(Bool())
    val pc = Output(UInt(32.W))
    val clk = Input(Clock())
    val pcstall = Input(Bool())
  })

  val pc_reg = RegInit(0x1c000000.U(32.W))

  when(!io.rst) {
    pc_reg := 0x1c000000.U
  } .elsewhen(io.pcstall) {
    pc_reg := pc_reg
  } .elsewhen(io.run) {
    pc_reg := io.npc
  } .otherwise {
    pc_reg := pc_reg
  }

  io.pc := pc_reg
}

object PC extends App {
  (new chisel3.stage.ChiselStage).emitVerilog(new PC)
}

import chisel3._
import chisel3.util._

class MUX3_1 extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(32.W))
    val b = Input(UInt(32.W))
    val c = Input(UInt(32.W))
    val sel = Input(UInt(2.W))
    val num = Output(UInt(32.W))
  })

  io.num := MuxCase(0.U, Array(
    (io.sel === "b00".U) -> io.a,
    (io.sel === "b01".U) -> io.b,
    (io.sel === "b10".U) -> io.c
  ))
}

object MUX3_1 extends App {
  (new chisel3.stage.ChiselStage).emitVerilog(new MUX3_1)
}

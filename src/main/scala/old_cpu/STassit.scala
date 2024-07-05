import chisel3._
import chisel3.util._

class STassist extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(32.W))  // 功能选择
    val b = Input(UInt(32.W))  // 被存数据
    val c = Input(UInt(32.W))  // 原数据
    val d = Output(UInt(32.W)) // 输出数据
    val e = Input(UInt(32.W))
  })

  // Initial output
  io.d := io.b

  // 更新d的值根据不同的指令
  when(io.a(1, 0) === "b11".U) { // b指令
    switch(io.e(1, 0)) {
      is("b00".U) { io.d := Cat(io.c(31, 8), io.b(7, 0)) }
      is("b01".U) { io.d := Cat(io.c(31, 16), io.b(7, 0), io.c(7, 0)) }
      is("b10".U) { io.d := Cat(io.c(31, 24), io.b(7, 0), io.c(15, 0)) }
      is("b11".U) { io.d := Cat(io.b(7, 0), io.c(23, 0)) }
    }
  }.elsewhen(io.a(1, 0) === "b10".U) { // H指令
    switch(io.e(1)) {
      is("b0".U) { io.d := Cat(io.c(31, 16), io.b(15, 0)) }
      is("b1".U) { io.d := Cat(io.b(15, 0), io.c(15, 0)) }
    }
  }.otherwise { // W指令
    io.d := io.b
  }
}

object STassist extends App {
  (new chisel3.stage.ChiselStage).emitVerilog(new STassist)
}

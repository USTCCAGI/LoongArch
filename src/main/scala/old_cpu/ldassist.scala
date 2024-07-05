import chisel3._
import chisel3.util._

class LDAssist extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(32.W))   // Function selection
    val b = Input(UInt(32.W))   // Data
    val d = Input(UInt(32.W))   // Address
    val c = Output(UInt(32.W))  // Output
  })

  val c_reg = RegInit(0.U(32.W))

  when(io.a(4, 2) === "b010".U) { // sinb instruction
    switch(io.d(1, 0)) {
      is("b00".U) {
        c_reg := Cat(Fill(24, io.b(7)), io.b(7, 0))
      }
      is("b01".U) {
        c_reg := Cat(Fill(24, io.b(15)), io.b(15, 8))
      }
      is("b10".U) {
        c_reg := Cat(Fill(24, io.b(23)), io.b(23, 16))
      }
      is("b11".U) {
        c_reg := Cat(Fill(24, io.b(31)), io.b(31, 24))
      }
      default {
        c_reg := io.b
      }
    }
  }.elsewhen(io.a(4, 2) === "b011".U) { // sinH instruction
    switch(io.d(1)) {
      is(0.U) {
        c_reg := Cat(Fill(16, io.b(15)), io.b(15, 0))
      }
      is(1.U) {
        c_reg := Cat(Fill(16, io.b(31)), io.b(31, 16))
      }
      default {
        c_reg := io.b
      }
    }
  }.elsewhen(io.a(4, 2) === "b001".U) { // ld.w instruction
    c_reg := io.b
  }.elsewhen(io.a(4, 2) === "b100".U) { // ld.bu instruction
    switch(io.d(1, 0)) {
      is("b00".U) {
        c_reg := Cat(Fill(24, 0.U), io.b(7, 0))
      }
      is("b01".U) {
        c_reg := Cat(Fill(24, 0.U), io.b(15, 8))
      }
      is("b10".U) {
        c_reg := Cat(Fill(24, 0.U), io.b(23, 16))
      }
      is("b11".U) {
        c_reg := Cat(Fill(24, 0.U), io.b(31, 24))
      }
      default {
        c_reg := io.b
      }
    }
  }.elsewhen(io.a(4, 2) === "b101".U) { // ld.hu instruction
    switch(io.d(1)) {
      is(0.U) {
        c_reg := Cat(Fill(16, 0.U), io.b(15, 0))
      }
      is(1.U) {
        c_reg := Cat(Fill(16, 0.U), io.b(31, 16))
      }
      default {
        c_reg := io.b
      }
    }
  }.otherwise {
    c_reg := io.b
  }

  io.c := c_reg
}

object LDAssist extends App {
  (new chisel3.stage.ChiselStage).emitVerilog(new LDAssist)
}

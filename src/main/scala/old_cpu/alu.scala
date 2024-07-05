import chisel3._
import chisel3.util._

class ALU extends Module {
  val io = IO(new Bundle {
    val a = Input(SInt(32.W))
    val b = Input(SInt(32.W))
    val alu_op = Input(UInt(12.W))
    val alu_result = Output(SInt(32.W))
  })

  io.alu_result := 0.S

  switch(io.alu_op) {
    is("h001".U) { io.alu_result := io.a + io.b }
    is("h002".U) { io.alu_result := io.a - io.b }
    is("h004".U) {
      when(io.a(31) ^ io.b(31)) {
        io.alu_result := Mux(io.a(31) === 1.U, 1.S, 0.S)
      }.otherwise {
        io.alu_result := (io.a(30, 0) < io.b(30, 0)).asSInt
      }
    }
    is("h008".U) {
      when(io.a(31) ^ io.b(31)) {
        io.alu_result := Mux(io.a(31) === 1.U, 0.S, 1.S)
      }.otherwise {
        io.alu_result := (io.a(30, 0) < io.b(30, 0)).asSInt
      }
    }
    is("h010".U) { io.alu_result := io.a & io.b }
    is("h020".U) { io.alu_result := io.a | io.b }
    is("h040".U) { io.alu_result := ~(io.a | io.b) }
    is("h080".U) { io.alu_result := io.a ^ io.b }
    is("h100".U) { io.alu_result := (io.a << io.b(4, 0)).asSInt }
    is("h200".U) { io.alu_result := (io.a >> io.b(4, 0)).asSInt }
    is("h400".U) { io.alu_result := (io.a.asUInt >> io.b(4, 0)).asSInt }
    is("h800".U) { io.alu_result := io.a * io.b }
    default { io.alu_result := io.a }
  }
}

object ALU extends App {
  (new chisel3.stage.ChiselStage).emitVerilog(new ALU)
}

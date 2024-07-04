import chisel3._
import chisel3.util._

class Forwarding extends Module {
  val io = IO(new Bundle {
    val clk = Input(Clock())
    val rst = Input(Bool())
    val RS1 = Input(UInt(6.W))
    val RS2 = Input(UInt(6.W))
    val irm = Input(UInt(6.W))
    val irw = Input(UInt(6.W))
    val WB_memwb = Input(Bool())
    val WB_exmem = Input(Bool())
    val afwd = Output(UInt(2.W))
    val bfwd = Output(UInt(2.W))
    val imm = Input(Bool())
  })

  val afwd_reg = RegInit(0.U(2.W))
  val bfwd_reg = RegInit(0.U(2.W))

  when(!io.rst) {
    afwd_reg := 0.U
    bfwd_reg := 0.U
  } .otherwise {
    when(io.irm === io.irw && io.irw === io.RS1 && (io.irw === 0.U || io.irw === 33.U) && io.RS1 === io.RS2) {
      afwd_reg := 0.U
      bfwd_reg := 0.U
    } .elsewhen(io.irm === io.irw && (io.irw === io.RS1 || io.irw === io.RS2)) {
      when(io.irw === io.RS1 && io.irw === io.RS2) {
        afwd_reg := 2.U
        bfwd_reg := 2.U
      } .elsewhen(io.irw === io.RS1) {
        afwd_reg := 2.U
        bfwd_reg := 0.U
      } .elsewhen(io.irw === io.RS2) {
        afwd_reg := 0.U
        bfwd_reg := 2.U
      } .otherwise {
        afwd_reg := 0.U
        bfwd_reg := 0.U
      }
    } .elsewhen(((io.irw === io.RS1 && io.irm === io.RS2) || (io.irw === io.RS2 && io.irm === io.RS1)) && !io.imm) {
      when(io.irm === io.RS1) {
        afwd_reg := 2.U
        bfwd_reg := 1.U
      } .elsewhen(io.irm === io.RS2) {
        afwd_reg := 1.U
        bfwd_reg := 2.U
      } .otherwise {
        afwd_reg := 0.U
        bfwd_reg := 0.U
      }
    } .elsewhen((io.irw === io.RS1 || io.irw === io.RS2) && (!(io.irm === io.RS1 || io.irm === io.RS2)) && io.irm =/= io.irw) {
      when(io.irw === io.RS1) {
        afwd_reg := 1.U
        bfwd_reg := 0.U
      } .elsewhen(io.irw === io.RS2) {
        afwd_reg := 0.U
        bfwd_reg := 1.U
      } .otherwise {
        afwd_reg := 0.U
        bfwd_reg := 0.U
      }
    } .elsewhen(io.irm === io.RS1 || io.irm === io.RS2) {
      when(io.irm === io.RS1) {
        afwd_reg := 2.U
        bfwd_reg := 0.U
      } .elsewhen(io.irm === io.RS2) {
        afwd_reg := 0.U
        bfwd_reg := 2.U
      } .otherwise {
        afwd_reg := 0.U
        bfwd_reg := 0.U
      }
    } .otherwise {
      afwd_reg := 0.U
      bfwd_reg := 0.U
    }
  }

  io.afwd := afwd_reg
  io.bfwd := bfwd_reg
}

object Forwarding extends App {
  (new chisel3.stage.ChiselStage).emitVerilog(new Forwarding)
}

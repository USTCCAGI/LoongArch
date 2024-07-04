import chisel3._
import chisel3.util._

class Branch extends Module {
  val io = IO(new Bundle {
    val br_type = Input(UInt(12.W))
    val pc = Input(UInt(32.W))
    val imm = Input(UInt(32.W))
    val rf_rdata1 = Input(UInt(32.W))
    val rf_rdata2 = Input(UInt(32.W))
    val jump_en = Output(Bool())
    val jump_target = Output(UInt(32.W))
    val ifidflush = Output(Bool())
    val idexflush = Output(Bool())
  })

  io.ifidflush := false.B
  io.idexflush := false.B

  val jump_en = Wire(Bool())
  val jump_target = Wire(UInt(32.W))

  switch(io.br_type(3, 0)) {
    is("b0000".U) {
      jump_en := false.B
      jump_target := 0.U
      io.ifidflush := false.B
      io.idexflush := false.B
    }
    is("b0001".U) { // bne
      when(io.rf_rdata1 =/= io.rf_rdata2) {
        jump_en := true.B
        jump_target := io.pc + io.imm
        io.ifidflush := true.B
        io.idexflush := true.B
      }.otherwise {
        jump_en := false.B
        jump_target := 0.U
        io.ifidflush := false.B
        io.idexflush := false.B
      }
    }
    is("b0010".U) {
      jump_en := true.B
      jump_target := io.rf_rdata2 + io.imm
      io.ifidflush := true.B
      io.idexflush := true.B
    }
    is("b0011".U) { // beq
      when(io.rf_rdata1 === io.rf_rdata2) {
        jump_en := true.B
        jump_target := io.pc + io.imm
        io.ifidflush := true.B
        io.idexflush := true.B
      }.otherwise {
        jump_en := false.B
        jump_target := 0.U
        io.ifidflush := false.B
        io.idexflush := false.B
      }
    }
    is("b0100".U) { // blt
      when(io.rf_rdata1(31) === io.rf_rdata2(31)) { // same sign
        when(io.rf_rdata1(30, 0) < io.rf_rdata2(30, 0)) {
          jump_en := true.B
          jump_target := io.pc + io.imm
          io.ifidflush := true.B
          io.idexflush := true.B
        }.otherwise {
          jump_en := false.B
          jump_target := 0.U
          io.ifidflush := false.B
          io.idexflush := false.B
        }
      }.elsewhen(io.rf_rdata1(31) === false.B) { // a > 0
        jump_en := false.B
        jump_target := 0.U
        io.ifidflush := false.B
        io.idexflush := false.B
      }.otherwise {
        jump_en := true.B
        jump_target := io.pc + io.imm
        io.ifidflush := true.B
        io.idexflush := true.B
      }
    }
    is("b0101".U) {
      when(io.rf_rdata1(31) === io.rf_rdata2(31)) { // same sign
        when(io.rf_rdata1(30, 0) >= io.rf_rdata2(30, 0)) {
          jump_en := true.B
          jump_target := io.pc + io.imm
          io.ifidflush := true.B
          io.idexflush := true.B
        }.otherwise {
          jump_en := false.B
          jump_target := 0.U
          io.ifidflush := false.B
          io.idexflush := false.B
        }
      }.elsewhen(io.rf_rdata1(31) === false.B) { // a > 0
        jump_en := true.B
        jump_target := io.pc + io.imm
        io.ifidflush := true.B
        io.idexflush := true.B
      }.otherwise {
        jump_en := false.B
        jump_target := 0.U
        io.ifidflush := false.B
        io.idexflush := false.B
      }
    }
    is("b0110".U) {
      when(io.rf_rdata1(31) =/= io.rf_rdata2(31)) { // XOR, opposite sign
        when(io.rf_rdata1(31)) { // a > b
          jump_en := false.B
          jump_target := 0.U
          io.ifidflush := false.B
          io.idexflush := false.B
        }.otherwise {
          jump_en := true.B
          jump_target := io.pc + io.imm
          io.ifidflush := true.B
          io.idexflush := true.B
        }
      }.otherwise {
        when(io.rf_rdata1(30, 0) < io.rf_rdata2(30, 0)) {
          jump_en := true.B
          jump_target := io.pc + io.imm
          io.ifidflush := true.B
          io.idexflush := true.B
        }.otherwise {
          jump_en := false.B
          jump_target := 0.U
          io.ifidflush := false.B
          io.idexflush := false.B
        }
      }
    }
    is("b0111".U) {
      when(io.rf_rdata1(31) =/= io.rf_rdata2(31)) { // XOR, opposite sign
        when(io.rf_rdata1(31) === false.B) { // a < b
          jump_en := false.B
          jump_target := 0.U
          io.ifidflush := false.B
          io.idexflush := false.B
        }.otherwise {
          jump_en := true.B
          jump_target := io.pc + io.imm
          io.ifidflush := true.B
          io.idexflush := true.B
        }
      }.otherwise {
        when(io.rf_rdata1(30, 0) >= io.rf_rdata2(30, 0)) {
          jump_en := true.B
          jump_target := io.pc + io.imm
          io.ifidflush := true.B
          io.idexflush := true.B
        }.otherwise {
          jump_en := false.B
          jump_target := 0.U
          io.ifidflush := false.B
          io.idexflush := false.B
        }
      }
    }
    is("b1000".U) {
      jump_en := true.B
      jump_target := io.pc + io.imm
      io.ifidflush := true.B
      io.idexflush := true.B
    }
    is("b1001".U) {
      jump_en := true.B
      jump_target := io.rf_rdata1 + io.imm
      io.ifidflush := true.B
      io.idexflush := true.B
    }
    is("b1010".U) {
      // Add more cases as needed...
    }
    // Add more cases as needed...
    // Default case
    default {
      jump_en := false.B
      jump_target := 0.U
      io.ifidflush := false.B
      io.idexflush := false.B
    }
  }

  io.jump_en := jump_en
  io.jump_target := jump_target
}

object Branch extends App {
  (new chisel3.stage.ChiselStage).emitVerilog(new Branch)
}

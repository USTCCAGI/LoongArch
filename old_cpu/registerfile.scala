import chisel3._
import chisel3.util._

class RegisterFile extends Module {
  val io = IO(new Bundle {
    val clk = Input(Clock())
    val rf_we = Input(Bool())
    val rf_raddr1 = Input(UInt(5.W))
    val rf_raddr2 = Input(UInt(5.W))
    val rf_raddr3 = Input(UInt(5.W))
    val rf_rdata1 = Output(UInt(32.W))
    val rf_rdata2 = Output(UInt(32.W))
    val rf_rdata3 = Output(UInt(32.W))
    val rf_rd = Input(UInt(5.W))
    val rd_wdata = Input(UInt(32.W))
  })

  val rf = RegInit(VecInit(Seq.fill(32)(0.U(32.W))))

  // Read logic
  io.rf_rdata1 := Mux(io.rf_rd === io.rf_raddr1 && io.rf_we, io.rd_wdata, rf(io.rf_raddr1))
  io.rf_rdata2 := Mux(io.rf_rd === io.rf_raddr2 && io.rf_we, io.rd_wdata, rf(io.rf_raddr2))
  io.rf_rdata3 := Mux(io.rf_rd === io.rf_raddr3 && io.rf_we, io.rd_wdata, rf(io.rf_raddr3))

  // Write logic
  when(io.rf_we && io.rf_rd =/= 0.U) {
    rf(io.rf_rd) := io.rd_wdata
  }
}

object RegisterFile extends App {
  (new chisel3.stage.ChiselStage).emitVerilog(new RegisterFile)
}

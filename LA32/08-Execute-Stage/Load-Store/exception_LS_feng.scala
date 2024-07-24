import Exception._
import chisel3._
import chisel3.util._

class Exception_LS_IO extends Bundle {
  val addr_EX = Input(UInt(32.W))
  val mem_type_EX = Input(UInt(5.W))
  val exception_ls = Output(UInt(8.W))
}

class Exception_LS extends Module {
  val io = IO(new Exception_LS_IO)
  val addr_EX = io.addr_EX
  val mem_type = io.mem_type_EX

  val exception_ls = Wire(UInt(8.W))
  val check = WireDefault(0.U(4.W))

  switch(mem_type(1, 0)) {
    is(0.U) { check := "b0001".U(4.W) }  // 1-byte
    is(1.U) { check := "b0011".U(4.W) }  // 2-byte
    is(2.U) { check := "b0111".U(4.W) }  // 4-byte
    is(3.U) { check := "b1111".U(4.W) }  // 8-byte
  }

  val alignment_exception = (addr_EX & check) =/= 0.U

  exception_ls := Mux(alignment_exception, 1.U(1.W) ## ALE, 0.U)

  io.exception_ls := exception_ls
}

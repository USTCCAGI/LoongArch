import chisel3._
import chisel3.util._
import Control_Signal._

//feng
class MDU_IO extends Bundle{
    val src1    = Input(UInt(32.W))
    val src2    = Input(UInt(32.W))
    val md_op   = Input(UInt(5.W))
    val mul_out = Output(UInt(32.W))
    val div_out = Output(UInt(32.W))
    val busy    = Output(Vec(32, Bool()))
}

class MDU extends Module{
    val io = IO(new MDU_IO)
    val mul = Module(new Multiply2)
    val div = Module(new Divide2)

    mul.io.num1 := io.src1
    mul.io.num2 := io.src2
    div.io.num1 := io.src1
    div.io.num2 := io.src2

    div.io.op := io.md_op
    mul.io.op := io.md_op

    io.busy := div.io.busy
    mul.io.busy := div.io.busy

    io.mul_out := mul.io.mul_out
    io.div_out := div.io.div_out

}
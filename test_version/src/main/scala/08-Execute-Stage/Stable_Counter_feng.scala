import chisel3._
import chisel3.util._

class Stable_Counter extends Module{
    val io = IO(new Bundle{
        val value = Output(UInt(64.W))
    })
    val count = RegInit(0.U(64.W))
    count := count + 1.U
    io.value := count
}
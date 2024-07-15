import chisel3._
import chisel3.util._
//feng
class Physical_Regfile_IO(n: Int) extends Bundle{
    // 8 read ports
    val prj       = Input(Vec(4, UInt(log2Ceil(n).W)))
    val prk       = Input(Vec(4, UInt(log2Ceil(n).W)))

    val prj_data  = Output(Vec(4, UInt(32.W)))
    val prk_data  = Output(Vec(4, UInt(32.W)))

    // 4 write ports
    val prd       = Input(Vec(4, UInt(log2Ceil(n).W)))
    val wdata     = Input(Vec(4, UInt(32.W)))
    val rf_we     = Input(Vec(4, Bool()))
}

class Physical_Regfile(n: Int) extends Module{
    val io = IO(new Physical_Regfile_IO(n))


    //寄存器堆
    val rf = RegInit(VecInit.fill(n)(0.U(32.W)))
    
    //
    val rf_we = io.rf_we
    val wdata = io.wdata
    val reg_write = io.prd

    //读取寄存器堆
    //写优先,prd是写地址，pr是读地址
    def write_first(rf_we: Vec[Bool], wdata: Vec[UInt], reg_write: Vec[UInt], pr: UInt, rf: Vec[UInt]): UInt = {
        val result = Wire(UInt()) // Define a wire for the result

        when(pr === reg_write(0)) {
          result := Mux(rf_we(0), wdata(0), rf(pr))
        }.elsewhen(pr === reg_write(1)) {
          result := Mux(rf_we(1), wdata(1), rf(pr))
        }.elsewhen(pr === reg_write(2)) {
          result := Mux(rf_we(2), wdata(2), rf(pr))
        }.elsewhen(pr === reg_write(3)) {
          result := Mux(rf_we(3), wdata(3), rf(pr))
        }.otherwise {
          result := rf(pr)
        }

        result // Return 
    }

    for(i <- 0 until 4){
        io.prj_data(i) := write_first(rf_we, wdata, reg_write, io.prj(i), rf)
        io.prk_data(i) := write_first(rf_we, wdata, reg_write, io.prk(i), rf)
    }

    //写入寄存器堆
    for(i <- 0 until 4){
        when(io.rf_we(i)){
            rf(io.prd(i)) := io.wdata(i)
        }
    }

}

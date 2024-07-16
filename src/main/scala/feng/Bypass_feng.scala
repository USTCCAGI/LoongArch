import chisel3._
import chisel3.util._
import CPU_Config._


class Bypass_3_IO extends Bundle {
    val prd_wb              = Input(Vec(3, UInt(log2Ceil(PREG_NUM).W)))//寄存器名，第4路的       
    val prj_ex              = Input(Vec(3, UInt(log2Ceil(PREG_NUM).W)))//寄存器名，第1路的       
    val prk_ex              = Input(Vec(3, UInt(log2Ceil(PREG_NUM).W)))//寄存器名，第2路的
    val prf_wdata_wb        = Input(Vec(3, UInt(32.W)))  //写数据
    val rd_valid_wb         = Input(Vec(3, Bool()))      //写有效
    val forward_prj_en      = Output(Vec(3, Bool()))
    val forward_prk_en      = Output(Vec(3, Bool()))
    val forward_prj_data    = Output(Vec(3, UInt(32.W)))
    val forward_prk_data    = Output(Vec(3, UInt(32.W)))
}

class Bypass_3 extends Module{
    val io = IO(new Bypass_3_IO)
    val rd_valid = io.rd_valid_wb
    val prd = io.prd_wb
    val prj = io.prj_ex
    val prk = io.prk_ex
    val prf = io.prf_wdata_wb

    for(i <- 0 until 2){
        when(rd_valid(0) || rd_valid(1)){ //出现写
            //如果rd_valid有效，且prd与prj相同，或者rd_valid有效，且prd与prk相同，forward有效，谁相同谁有效谁前递
            io.forward_prj_en(i) := ((rd_valid(0) && !(prd(0) ^ prj(i))) || (rd_valid(1) && !(prd(1) ^ prj(i))))
            io.forward_prk_en(i) := ((rd_valid(0) && !(prd(0) ^ prk(i))) || (rd_valid(1) && !(prd(1) ^ prk(i))))
            when(io.forward_prj_en(i)){
                io.forward_prj_data(i) := Mux(!(prd(0) ^ prj(i)), prf(0), prf(1))
            }.otherwise{
                io.forward_prj_data(i) := 0.U
            }
            when(io.forward_prk_en(i)){
                io.forward_prk_data(i) := Mux(!(prd(0) ^ prk(i)), prf(0), prf(1))
            }.otherwise{
                io.forward_prk_data(i) := 0.U
            }
        }.otherwise{   //不出现写，则肯定不前递
            io.forward_prj_en(i) := false.B
            io.forward_prk_en(i) := false.B
            io.forward_prj_data(i) := 0.U
            io.forward_prk_data(i) := 0.U
        }
    }
    //第四路涉及访存，如果rd_valid有效，第四路的输入寄存器和第一路的输入寄存器相同，或者第四路的和第二路相同，或第四路和第四路相同，forward有效，谁相同谁有效谁前递
    
    io.forward_prj_en(2) := (rd_valid(0) && !(prd(0) ^ prj(2))) || (rd_valid(1) && !(prd(1) ^ prj(2))) || (rd_valid(2) && !(prd(2) ^ prj(2)))
    io.forward_prk_en(2) := (rd_valid(0) && !(prd(0) ^ prk(2))) || (rd_valid(1) && !(prd(1) ^ prk(2))) || (rd_valid(2) && !(prd(2) ^ prk(2)))
    when(io.forward_prj_en(2)){
        when(prd(0) === prj(2)){
            io.forward_prj_data(2) := prf(0)
        }.elsewhen(prd(1) === prj(2)){
            io.forward_prj_data(2) := prf(1)
        }.otherwise{
            io.forward_prj_data(2) := prf(2)
        }
    }.otherwise{
        io.forward_prj_data(2) := 0.U
    }
    when(io.forward_prk_en(2)){
        when(prd(0) === prk(2)){
            io.forward_prk_data(2) := prf(0)
        }.elsewhen(prd(1) === prk(2)){
            io.forward_prk_data(2) := prf(1)
        }.otherwise{
            io.forward_prk_data(2) := prf(2)
        }
    }.otherwise{
        io.forward_prk_data(2) := 0.U
    }

}





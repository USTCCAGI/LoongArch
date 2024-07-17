import chisel3._
import chisel3.util._
import CPU_Config._

class Reg_rename_IO(n: Int) extends Bundle{
    val rj                  = Input(Vec(2, UInt(5.W)))
    val rk                  = Input(Vec(2, UInt(5.W)))

    val rd                  = Input(Vec(2, UInt(5.W)))
    val rd_valid            = Input(Vec(2, Bool()))
    val rename_en           = Input(Vec(2, Bool()))

    val alloc_preg          = Input(Vec(2, UInt(log2Ceil(n).W)))

    val prj                 = Output(Vec(2, UInt(log2Ceil(n).W)))
    val prk                 = Output(Vec(2, UInt(log2Ceil(n).W)))
    val prd                 = Output(Vec(2, UInt(log2Ceil(n).W)))
    val pprd                = Output(Vec(2, UInt(log2Ceil(n).W)))

    val predict_fail        = Input(Bool())
    val arch_rat            = Input(Vec(n, UInt(1.W)))

    val prj_ready           = Output(Vec(2, Bool()))
    val prk_ready           = Output(Vec(2, Bool()))

    val prd_wake            = Input(Vec(4, UInt(log2Ceil(n).W)))
    val wake_valid          = Input(Vec(4, Bool()))

}

class Reg_Rename(n: Int) extends Module{
    val io = IO(new Reg_rename_IO(n))
    val crat = Module(new CRat(n))

    val phy_rj_temp = crat.io.prj
    val phy_rk_temp = crat.io.prk
    val phy_rd_temp = crat.io.pprd

    val rj_raw = io.rd_valid(0) && (io.rd(0) === io.rj(1))
    val rk_raw = io.rd_valid(0) && (io.rd(0) === io.rk(1))

    io.prd := io.alloc_preg 

    //CRAT
    val rd_valid_temp = Wire(Vec(2, Bool()))
    rd_valid_temp(1) := io.rd_valid(1)
    rd_valid_temp(0) := io.rd_valid(0) && !((io.rd(0) === io.rd(1)) && io.rd_valid(1))


    crat.io.rj := io.rj
    crat.io.rk := io.rk
    crat.io.rd := io.rd
    crat.io.rd_valid := (rd_valid_temp.asUInt & io.rename_en.asUInt).asBools
    crat.io.alloc_preg := io.alloc_preg
    crat.io.arch_rat := io.arch_rat
    crat.io.predict_fail := io.predict_fail
    crat.io.prd_wake := io.prd_wake
    crat.io.wake_valid := io.wake_valid
    

    // RAW
    io.prj(0) := crat.io.prj(0)
    io.prj(1) := Mux(rj_raw, io.alloc_preg(0), crat.io.prj(1))
    io.prj_ready(0) := crat.io.prj_ready(0)
    io.prj_ready(1) := Mux(rj_raw, false.B, crat.io.prj_ready(1))

    io.prk(0) := crat.io.prk(0)
    io.prk(1) := Mux(rk_raw, io.alloc_preg(0), crat.io.prk(1))
    io.prk_ready(0) := crat.io.prk_ready(0)
    io.prk_ready(1) := Mux(rk_raw, false.B, crat.io.prk_ready(1))


    // WAW
    val rd_waw = io.rd_valid(0) && (io.rd(0) === io.rd(1))
    io.pprd(0) := crat.io.pprd(0)
    io.pprd(1) := Mux(rd_waw, io.alloc_preg(0), crat.io.pprd(1))
}


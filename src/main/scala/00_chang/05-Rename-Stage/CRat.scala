import chisel3._
import chisel3.util._
import CPU_Config._
import Rat._

class CRat_IO(n: Int) extends Bundle{
    val rj           = Input(Vec(2, UInt(5.W)))
    val rk           = Input(Vec(2, UInt(5.W)))

    val rd           = Input(Vec(2, UInt(5.W)))
    val rd_valid     = Input(Vec(2, Bool()))
    val alloc_preg   = Input(Vec(2, UInt(log2Ceil(n).W)))

    val prj          = Output(Vec(2, UInt(log2Ceil(n).W)))
    val prk          = Output(Vec(2, UInt(log2Ceil(n).W)))
    val pprd         = Output(Vec(2, UInt(log2Ceil(n).W)))

    val arch_rat     = Input(Vec(n, UInt(1.W)))
    val predict_fail = Input(Bool())

    // read by inst
    val prj_ready     = Output(Vec(2, Bool()))
    val prk_ready     = Output(Vec(2, Bool()))

    // write by wakeup
    val prd_wake     = Input(Vec(4, UInt(log2Ceil(n).W)))
    val wake_valid   = Input(Vec(4, Bool()))

}

class CRat(n: Int) extends Module{
    val io = IO(new CRat_IO(n))
    val arch_rj = io.rj
    val arch_rk = io.rk
    val arch_rd = io.rd
    val arch_rd_valid = io.rd_valid
    val alloc_phy_rd = io.alloc_preg
    val arch_rat = io.arch_rat
    val predict_fail = io.predict_fail
    val prd_wake = io.prd_wake
    val wake_valid = io.wake_valid

    val crat = RegInit(VecInit.fill(n)(0.U.asTypeOf(new rat_t))) //crat initailized
    // read physical registers for arch_rj and arch_rk and arch_rd
    for(i <- 0 until 2){
        val phy_rj_OH = crat.map(entry => entry.valid && entry.lr === arch_rj(i))
        val phy_rk_OH = crat.map(entry => entry.valid && entry.lr === arch_rk(i))
        val phy_rd_OH = crat.map(entry => entry.valid && entry.lr === arch_rd(i))

        io.prj(i)   := OHToUInt(VecInit(phy_rj_OH))
        io.prk(i)   := OHToUInt(VecInit(phy_rk_OH))
        io.pprd(i)  := OHToUInt(VecInit(phy_rd_OH))

        io.prj_ready(i) := crat(OHToUInt(phy_rj_OH)).free
        io.prk_ready(i) := crat(OHToUInt(phy_rk_OH)).free
    }


    //write for dispatch
    when(predict_fail){
        crat.foreach { entry =>
            entry.valid := false.B
            entry.free := true.B
        }
    }.otherwise{
        for(i <- 0 until 2){
            when(arch_rd_valid(i)){
                val phy_rd = alloc_phy_rd(i)
                crat(phy_rd).lr := arch_rd(i)
                crat(phy_rd).valid := true.B
                crat(phy_rd).free := false.B
                crat(io.pprd(i)).valid := false.B   // clear the valid bit of the physical register that was once assigned to rd
            }
        }
        for(i <- 0 until 4){
            when(wake_valid(i)){
                crat(prd_wake(i)).free := true.B
            }
        }
    }

}
import chisel3._
import chisel3.util._
import CPU_Config._

class PC_IO extends Bundle {
    val pc_PF           = Output(Vec(10, UInt(32.W)))//传给下级的pc，有扇出优化，实际用5条
    val pc_stall        = Input(Bool())
    val predict_fail    = Input(Bool())
    val npc             = Output(Vec(10, UInt(32.W)))
    val pred_jump       = Input(Vec(2, Bool()))
    val pred_npc        = Input(UInt(32.W))
    val branch_target   = Input(UInt(32.W))
    val inst_valid_PF   = Output(Vec(2, Bool()))
    val exception_PF    = Output(UInt(8.W))//

    val flush_by_pd     = Input(Bool())
    val flush_pd_target = Input(UInt(32.W))

    // idle
    val is_idle_cmt     = Input(Bool())
    val has_intr        = Input(Bool())

    // csr change
    val has_csr_change  = Input(Bool())

    // pc plus
    //val pc_plus_1_28    = Output(UInt(32.W))//未见到这些信号被使用
    //val pc_minus_1_28   = Output(UInt(32.W))//未见到这些信号被使用
    //val pc_plus_1_18    = Output(UInt(32.W))//未见到这些信号被使用
    //val pc_minus_1_18   = Output(UInt(32.W))//未见到这些信号被使用
}
class PC(reset_val:Int) extends Module{
    val io = IO(new PC_IO)

    val pc = RegInit(VecInit.fill(10)(reset_val.U(32.W)))
    val run = RegInit(false.B)

    run := Mux(io.has_intr, false.B, Mux(!run, io.is_idle_cmt, run))

    for(i<-0 until 10){
        when(run || io.has_csr_change ||io.pc_stall){
            io.npc(i) := pc(i)
        }.elsewhen(io.predict_fail){//分支预测失败
            io.npc(i) := io.branch_target
        }.elsewhen(io.flush_by_pd){//分支预测修改
            io.npc(i) := io.flush_pd_target
        }.otherwise{//pc不停
            when(io.pred_jump.asUInt.orR){//强制jump信号
                io.npc(i) := io.pred_npc
            }.otherwise{//pc+8
                io.npc(i) := (pc(i) + 8.U)(31, 3) ## 0.U(3.W)
            }
        }

        io.pc_PF(i) := pc(i)//取指用
        pc(i) := io.npc(i)
    }
    when(run || io.has_csr_change){
        io.inst_valid_PF(0) := false.B
        io.inst_valid_PF(1) := false.B
    }.otherwise{
        io.inst_valid_PF(0) := true.B
        io.inst_valid_PF(1) := !io.pred_jump(0) && !pc(1)(2)
    }
    io.exception_PF := Mux(pc(1)(1, 0) === 0.U, 0.U, 0x88.U)//地址错例外

}
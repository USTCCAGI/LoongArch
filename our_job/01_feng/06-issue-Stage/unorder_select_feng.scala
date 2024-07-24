import chisel3._
import chisel3.util._
import Inst_Pack._
import Issue_Queue_Struct._
import CPU_Config._

class Unorder_Select_IO[T <: inst_pack_DP_t](n: Int, inst_pack_t: T) extends Bundle {
    val insts_issue         = Input(Vec(n, new issue_queue_t(inst_pack_t)))//发射队列传来的指令包
    val issue_req           = Input(Vec(n, Bool()))//发射队列的请求
    val stall               = Input(Bool())//是否stall

    val issue_ack           = Output(Vec(n, Bool())) //发射队列传来的ack
    val wake_preg           = Output(UInt(log2Ceil(PREG_NUM).W))  //通知发射队列是否唤醒

    val inst_issue          = Output(new issue_queue_t(inst_pack_t)) //给下级传去的指令
    val inst_issue_valid    = Output(Bool())   //指令有效否
}

class Unorder_Select[T <: inst_pack_DP_t](n: Int, inst_pack_t: T) extends Module {
    val io                  = IO(new Unorder_Select_IO(n, inst_pack_t))

    val select     = PriorityEncoderOH(io.issue_req)
    val inst       = Mux1H(select, io.insts_issue)
    val choice     = io.issue_req.asUInt.orR && !io.stall  //是否可选
    when(choice === 1.U){
        //可选 默认只要有请求，就有指令被发射
        io.issue_ack := VecInit(select)
        io.inst_issue_valid := 1.U
        io.inst_issue := inst
        io.wake_preg  := Mux1H(select, io.insts_issue.map(_.inst.asInstanceOf[inst_pack_DP_t].prd))

    }.otherwise{
        //不可选
        io.issue_ack := VecInit(Seq.fill(n)(0.U))
        io.inst_issue_valid := 0.U
        io.inst_issue := 0.U.asTypeOf(new issue_queue_t(inst_pack_t))
        io.wake_preg := 0.U
    }
}

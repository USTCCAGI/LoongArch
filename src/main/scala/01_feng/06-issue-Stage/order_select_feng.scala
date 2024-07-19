import chisel3._
import chisel3.util._
import Inst_Pack._
import Issue_Queue_Struct._
import CPU_Config._

class Order_Select_IO[T <: inst_pack_DP_t](n: Int, inst_pack_t: T) extends Bundle {
    val insts_issue         = Input(new issue_queue_t(inst_pack_t))//发射队列传来的指令
    val issue_req           = Input(Bool())//发射队列的请求
    val stall               = Input(Bool())//是否stall

    val issue_ack           = Output(Bool()) //发射队列传来的ack
    val wake_preg           = Output(UInt(log2Ceil(PREG_NUM).W))  //通知发射队列是否唤醒

    val inst_issue          = Output(new issue_queue_t(inst_pack_t)) //给下级传去的指令
    val inst_issue_valid    = Output(Bool())   //指令有效否
}

class Order_Select[T <: inst_pack_DP_t](n: Int, inst_pack_t: T) extends Module {
    val io                  = IO(new Order_Select_IO(n, inst_pack_t))

    val inst       = io.insts_issue
    val choice     = io.issue_req.asUInt.orR && !io.stall  //是否可选
    when(choice === 1.U){
        //可选 默认只要有请求，io收到的指令直接发射
        io.issue_ack := 1.U
        io.inst_issue_valid := 1.U
        io.inst_issue := inst
        //给出的wake信号，指示哪个目标寄存器prd被占用
        io.wake_preg  := Mux(io.insts_issue.inst.asInstanceOf[inst_pack_DP_t].rd_valid, io.insts_issue.inst.asInstanceOf[inst_pack_DP_t].prd,0.U)

    }.otherwise{
        //不可选
        io.issue_ack := 0.U
        io.inst_issue_valid := 0.U
        io.inst_issue := 0.U.asTypeOf(new issue_queue_t(inst_pack_t))
        io.wake_preg := 0.U
    }
}

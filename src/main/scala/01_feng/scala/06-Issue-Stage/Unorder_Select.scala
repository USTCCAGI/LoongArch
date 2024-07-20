import chisel3._
import chisel3.util._
import Inst_Pack._
import Issue_Queue_Struct._
import CPU_Config._
//n是发送队列的长度，inst_pack_t是指令包的类型，从发射队列挑选符合情况的指令，送到后边去。

class Unorder_Select_IO[T <: inst_pack_DP_t](n: Int, inst_pack_t: T) extends Bundle {
    val insts_issue         = Input(Vec(n, new issue_queue_t(inst_pack_t)))//发射队列传来的指令包
    val issue_req           = Input(Vec(n, Bool()))//发射队列的请求
    val stall               = Input(Bool())//是否stall

    val issue_ack           = Output(Vec(n, Bool())) //发射队列传来的ack
    val wake_preg           = Output(UInt(log2Ceil(PREG_NUM).W))  //通知发射队列是否唤醒
    // val priv_issued         = Output(Bool())

    val inst_issue          = Output(new issue_queue_t(inst_pack_t)) //给下级传去的指令
    val inst_issue_valid    = Output(Bool())   //指令有效否
}

class Unorder_Select[T <: inst_pack_DP_t](n: Int, inst_pack_t: T) extends Module {
    val io                  = IO(new Unorder_Select_IO(n, inst_pack_t))

    // val select_index        = PriorityEncoder(io.issue_req)
    // val issue_ack           = UIntToOH(select_index)(n-1, 0)
    // val issue_ack_vec       = VecInit(issue_ack.asBools)
    // io.issue_ack            := Mux(io.issue_req.asUInt.orR && !io.stall, issue_ack_vec, 0.U.asTypeOf(Vec(n, Bool())))

    
    // io.wake_preg            := Mux(io.issue_ack.asUInt.orR, io.insts_issue(select_index).inst.asInstanceOf[inst_pack_DP_t].prd, 0.U)
    
    // val inst_issue          = io.insts_issue(select_index)
    // val bubble_inst_issue   = 0.U.asTypeOf(new issue_queue_t(inst_pack_t))
    // io.inst_issue           := Mux(io.issue_ack.asUInt.orR, inst_issue, bubble_inst_issue)
    // io.inst_issue_valid     := io.issue_ack.asUInt.orR

    val select_indexOH        = PriorityEncoderOH(io.issue_req)
    val issue_ack             = select_indexOH
    val issue_ack_vec         = VecInit(issue_ack)
    //给队列的issue——ack信号，指示哪个指令被发射
    io.issue_ack              := Mux(io.issue_req.asUInt.orR && !io.stall, issue_ack_vec, 0.U.asTypeOf(Vec(n, Bool())))
    //给出的wake信号，指示哪个目标寄存器prd被占用
    io.wake_preg              := Mux(io.issue_ack.asUInt.orR, Mux1H(select_indexOH, io.insts_issue.map(_.inst.asInstanceOf[inst_pack_DP_t].prd)), 0.U)

    //给下级传递的指令
    val inst_issue            = Mux1H(select_indexOH, io.insts_issue)
    val bubble_inst_issue     = 0.U.asTypeOf(new issue_queue_t(inst_pack_t))
    io.inst_issue             := Mux(io.issue_ack.asUInt.orR, inst_issue, bubble_inst_issue)
    io.inst_issue_valid       := io.issue_ack.asUInt.orR

}


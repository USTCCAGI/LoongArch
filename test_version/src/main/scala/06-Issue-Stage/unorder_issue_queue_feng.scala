import chisel3._
import chisel3.util._
import Inst_Pack._
import Control_Signal._
import CPU_Config._
import Issue_Queue_Struct._


class Unorder_Issue_Queue_IO[T <: inst_pack_DP_t](n: Int, inst_pack_t: T) extends Bundle{
    // depatch模块接入，2条depatch指令，每条指令包含2个inst_pack_t
    //由index和valid组成的掩码，指示当前周期需要进入发射队列的指令位于指令组的哪个部分
    val insts_disp_index = Input(Vec(2, UInt(1.W)))
    val insts_disp_valid = Input(Vec(2, Bool()))
    val insts_dispatch   = Input(Vec(2, inst_pack_t))
    //寄存器是否可用
    val prj_ready        = Input(Vec(2, Bool()))
    val prk_ready        = Input(Vec(2, Bool()))

    //prg寄存器是否可用
    val wake_preg        = Input(Vec(4, UInt(log2Ceil(PREG_NUM).W)))
    //load的prd寄存器是否可用
    val ld_mem_prd       = Input(UInt(log2Ceil(PREG_NUM).W))

    // select模块的确认信号，n为队列长度
    val issue_ack        = Input(Vec(n, Bool()))

    // 给select发送指令包
    val insts_issue      = Output(Vec(n, new issue_queue_t(inst_pack_t)))
    val issue_req        = Output(Vec(n, Bool()))

    //给depatch发送信号，元素数量，是否队列满
    val elem_num         = Output(UInt((log2Ceil(n)+1).W))
    val full             = Output(Bool())
    
    //输入是否停止，是否flush，是否·由·dcachemiss
    val stall            = Input(Bool())
    val flush            = Input(Bool())
    val dcache_miss      = Input(Bool())
}

class Unorder_Issue_Queue[T <: inst_pack_DP_t](n: Int, inst_pack_t: T) extends Module{
    val io              = IO(new Unorder_Issue_Queue_IO(n, inst_pack_t))
    //队列声明
    val queue = RegInit(VecInit.fill(n)(0.U.asTypeOf(new issue_queue_t(inst_pack_t))))
    val num  = RegInit(0.U((log2Ceil(n)+1).W)) //队列中有几条指令
    io.elem_num := num
    val qvalid = RegInit(0.U(n.W))    //队列掩码,从低位开始，有指令为1
    val empty = !qvalid(0) //队列是否为空
    val full = qvalid(n-2)   //队列是否满
    io.full := full
    val ins_num = PopCount(io.insts_disp_valid) //插入的指令数,00 01 10 11
    

    //队列操作
    //左移1，进入一条
    def insert1(x: UInt): UInt = {
        val n = x.getWidth
        x(n-2, 0) ## 1.U(1.W)
    }
    //左移2，进入两条
    def insert2(x: UInt): UInt = {
        val n = x.getWidth
        x(n-3, 0) ## 3.U(2.W)
    }
    //右移1，发射一条
    def pop1(x: UInt): UInt = {
        val n = x.getWidth
        0.U(1.W) ## x(n-1, 1)
    }
    val num_pop = num - io.issue_ack.asUInt.orR
    val qvalid_pop = Mux(io.issue_ack.asUInt.orR, pop1(qvalid), qvalid)

    //与select交互
    //从select过来ack，即发送了哪条指令
    val issue = ~(io.issue_ack.asUInt - 1.U)  //issue为1的位在下面右移
    //发送内容
    io.insts_issue := queue
    
    //load的prd，影响输出给select的值
    val ld_mem_prd_valid    = io.ld_mem_prd.orR
    val ld_wake_prd_valid   = io.wake_preg(3).orR

    //队列重整，每次issue指示发送了哪条指令后面有多少要移
    for(i <- 0 until n){
        //包含队列内容移动的过程
        val queue_next = Wire(new issue_queue_t(inst_pack_t))
        val mem_prd         = io.wake_preg(3)
        val mem_prd_valid   = ld_wake_prd_valid
        when(qvalid_pop(i)){
            queue_next := (if(i == n-1) queue(i) else Mux(issue(i), queue(i+1), queue(i)))
        }.otherwise{
            val index = (i.U - num_pop)(0)
            //index是插入位置，先0后1，索引valid有效的指令先放
            when(!(!io.insts_disp_valid(0) && io.insts_disp_valid(1))){
                //10  第一条指令无效，第二条有效  11 两条指令都有效  00
                queue_next.inst := io.insts_dispatch(index)
                queue_next.prj_waked := io.prj_ready(index)
                queue_next.prk_waked := io.prk_ready(index)
                queue_next.prj_wake_by_ld := !(io.insts_dispatch(index).prj ^ io.ld_mem_prd) && ld_mem_prd_valid
                queue_next.prk_wake_by_ld := !(io.insts_dispatch(index).prk ^ io.ld_mem_prd) && ld_mem_prd_valid
            }.otherwise{
                //01  第一条指令有效，第二条无效
                queue_next.inst := io.insts_dispatch(1)
                queue_next.prj_waked := io.prj_ready(1)
                queue_next.prk_waked := io.prk_ready(1)
                queue_next.prj_wake_by_ld := !(io.insts_dispatch(1).prj ^ io.ld_mem_prd) && ld_mem_prd_valid
                queue_next.prk_wake_by_ld := !(io.insts_dispatch(1).prk ^ io.ld_mem_prd) && ld_mem_prd_valid
             
            }
        }
        queue(i).inst           := queue_next.inst
        queue(i).prj_waked      := queue_next.prj_waked || Wake_Up(io.wake_preg, queue_next.inst.asInstanceOf[inst_pack_DP_t].prj)
        queue(i).prk_waked      := queue_next.prk_waked || Wake_Up(io.wake_preg, queue_next.inst.asInstanceOf[inst_pack_DP_t].prk)
        queue(i).prj_wake_by_ld := (!(queue_next.inst.asInstanceOf[inst_pack_DP_t].prj ^ mem_prd) && mem_prd_valid) || queue_next.prj_wake_by_ld
        queue(i).prk_wake_by_ld := (!(queue_next.inst.asInstanceOf[inst_pack_DP_t].prk ^ mem_prd) && mem_prd_valid) || queue_next.prk_wake_by_ld
    }
    //
    when(io.flush === true.B){
        num := 0.U
        qvalid := 0.U
    }.otherwise{
        when(io.stall === true.B){
            num := num_pop
            qvalid := qvalid_pop
        }.otherwise{
            when(full === 0.U){//如果没满，还可以加
                num := num_pop + ins_num
            }.otherwise{
                num := num_pop
            }
            qvalid := MuxLookup(ins_num, qvalid_pop)(Seq(
                    0.U -> qvalid_pop,
                    1.U -> insert1(qvalid_pop),
                    2.U -> insert2(qvalid_pop)))
        }
    }
    //输出
    for(i<-0 until n){
         io.issue_req(i) := (qvalid(i) && queue(i).prj_waked && queue(i).prk_waked 
                             && !((queue(i).prj_wake_by_ld && !(queue(i).inst.prj ^ io.ld_mem_prd) 
                                || queue(i).prk_wake_by_ld && !(queue(i).inst.prk ^ io.ld_mem_prd)) && io.dcache_miss))
    }
}
import chisel3._
import chisel3.util._
import Inst_Pack._
import Control_Signal._
import CPU_Config._
import Issue_Queue_Struct._

class Unorder_Issue_Queue_IO[T <: inst_pack_DP_t](n: Int, inst_pack_t: T) extends Bundle{
    // input from dispatch
    val insts_disp_index = Input(Vec(2, UInt(1.W)))  //wu
    val insts_disp_valid = Input(Vec(2, Bool()))
    val insts_dispatch   = Input(Vec(2, inst_pack_t))
    val prj_ready        = Input(Vec(2, Bool()))
    val prk_ready        = Input(Vec(2, Bool()))
    val queue_ready      = Output(Bool())   //内部线，无外出
    // input from wakeup
    val wake_preg        = Input(Vec(4, UInt(log2Ceil(PREG_NUM).W)))
    // input from load
    val ld_mem_prd       = Input(UInt(log2Ceil(PREG_NUM).W))
    // val is_ld_mem        = Input(Bool())

    // input for issue ack
    val issue_ack        = Input(Vec(n, Bool()))

    // output for issue
    val insts_issue      = Output(Vec(n, new issue_queue_t(inst_pack_t)))
    val issue_req        = Output(Vec(n, Bool()))

    // output for dispatch
    val elem_num         = Output(UInt((log2Ceil(n)+1).W))  //== tail
    val full             = Output(Bool())
    // input from stall and flush
    val stall            = Input(Bool())
    val flush            = Input(Bool())
    val dcache_miss      = Input(Bool())
}

class Unorder_Issue_Queue[T <: inst_pack_DP_t](n: Int, inst_pack_t: T) extends Module{
    val io              = IO(new Unorder_Issue_Queue_IO(n, inst_pack_t))
    val queue           = RegInit(VecInit.fill(n)(0.U.asTypeOf(new issue_queue_t(inst_pack_t))))
    def shift_add1(x: UInt): UInt = { //左移1，进入一条
        val n = x.getWidth
        x(n-2, 0) ## 1.U(1.W)
    }
    def shift_sub1(x: UInt): UInt = {    //右移1 ，发射一条
        val n = x.getWidth
        0.U(1.W) ## x(n-1, 1)
    }
    def shift_add2(x: UInt): UInt = {  //左移2，相当于进入两条指令
        val n = x.getWidth
        x(n-3, 0) ## 3.U(2.W)
    }
    val tail            = RegInit(0.U((log2Ceil(n)+1).W))
    val qmask           = RegInit(0.U(n.W))

    // val empty           = tail === 0.U
    val empty           = !qmask(0)
    val insert_num      = PopCount(io.insts_disp_valid)
    val tail_pop        = tail - io.issue_ack.asUInt.orR
    val qmask_pop       = Mux(io.issue_ack.asUInt.orR, shift_sub1(qmask), qmask)
    // val full            = tail > (n-2).U
    val full            = qmask(n-2)
    

    val insts_dispatch  = io.insts_dispatch
    val disp_index      = io.insts_disp_index
    // val queue_next      = Wire(Vec(n, new issue_queue_t(inst_pack_t)))
    
    // issue，找到发射的那条指令
    val next_mask       = ~(io.issue_ack.asUInt - 1.U)

    val ld_mem_prd_valid    = io.ld_mem_prd.orR
    val ld_wake_prd_valid   = io.wake_preg(3).orR
    for(i <- 0 until n){
        val queue_next      = Wire(new issue_queue_t(inst_pack_t))
        val mem_prd         = io.wake_preg(3)
        val mem_prd_valid   = ld_wake_prd_valid
        when(qmask_pop(i)){
            queue_next := (if(i == n-1) queue(i) else Mux(next_mask(i), queue(i+1), queue(i)))
        }.otherwise{
            val idx                     = (i.U - tail_pop)(0)
            queue_next.inst             := io.insts_dispatch(disp_index(idx))
            queue_next.prj_waked        := io.prj_ready(disp_index(idx))
            queue_next.prk_waked        := io.prk_ready(disp_index(idx))
            queue_next.prj_wake_by_ld   := !(io.insts_dispatch(disp_index(idx)).prj ^ io.ld_mem_prd) && ld_mem_prd_valid
            queue_next.prk_wake_by_ld   := !(io.insts_dispatch(disp_index(idx)).prk ^ io.ld_mem_prd) && ld_mem_prd_valid
        }
        queue(i).inst           := queue_next.inst
        queue(i).prj_waked      := queue_next.prj_waked || Wake_Up(io.wake_preg, queue_next.inst.asInstanceOf[inst_pack_DP_t].prj)
        queue(i).prk_waked      := queue_next.prk_waked || Wake_Up(io.wake_preg, queue_next.inst.asInstanceOf[inst_pack_DP_t].prk)
        queue(i).prj_wake_by_ld := (!(queue_next.inst.asInstanceOf[inst_pack_DP_t].prj ^ mem_prd) && mem_prd_valid) || queue_next.prj_wake_by_ld
        queue(i).prk_wake_by_ld := (!(queue_next.inst.asInstanceOf[inst_pack_DP_t].prk ^ mem_prd) && mem_prd_valid) || queue_next.prk_wake_by_ld
    }
    tail    := Mux(io.flush, 0.U, Mux(io.stall, tail_pop, tail_pop + Mux(io.queue_ready, insert_num, 0.U)))
    qmask   := Mux(io.flush, 0.U, Mux(io.stall, qmask_pop, 
                MuxLookup(insert_num, qmask_pop)(Seq(
                    0.U -> qmask_pop,
                    1.U -> shift_add1(qmask_pop),
                    2.U -> shift_add2(qmask_pop)))))

    // output
    for(i <- 0 until n){
        // if(inst_pack_t.isInstanceOf[inst_pack_DP_LS_t]){
        //     if(i == 0){
        //         io.issue_req(i) := i.asUInt < tail && queue(i).prj_waked && queue(i).prk_waked
        //     }
        //     else{
        //         val mem_type = queue(i).inst.asInstanceOf[inst_pack_DP_LS_t].mem_type
        //         when(mem_type(4)){
        //             io.issue_req(i)     := false.B
        //         }.otherwise{
        //             val mem_type_ahead  = VecInit(queue.map(_.inst.asInstanceOf[inst_pack_DP_LS_t].mem_type).take(i))
        //             val store_ahead     = VecInit.tabulate(i)(j => mem_type_ahead(j)(4)).asUInt.orR
        //             io.issue_req(i)     := (i.asUInt < tail && queue(i).prj_waked && queue(i).prk_waked) && !store_ahead
        //         }
        //     }
        // }
        // else {
            // io.issue_req(i) := (i.asUInt < tail && queue(i).prj_waked && queue(i).prk_waked 
            //                 && !((queue(i).prj_wake_by_ld && !(queue(i).inst.prj ^ io.ld_mem_prd) 
            //                    || queue(i).prk_wake_by_ld && !(queue(i).inst.prk ^ io.ld_mem_prd)) && io.dcache_miss))
             io.issue_req(i) := (qmask(i) && queue(i).prj_waked && queue(i).prk_waked 
                             && !((queue(i).prj_wake_by_ld && !(queue(i).inst.prj ^ io.ld_mem_prd) 
                                || queue(i).prk_wake_by_ld && !(queue(i).inst.prk ^ io.ld_mem_prd)) && io.dcache_miss))
        // }
        
    }
    io.insts_issue      := queue
    io.full             := full
    io.queue_ready      := !full
    io.elem_num         := tail

}


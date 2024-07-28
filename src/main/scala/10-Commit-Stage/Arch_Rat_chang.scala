import chisel3._
import chisel3.util._
import Rat._
import Predict_Config._
import CPU_Config._

class Arch_Rat_IO(n: Int) extends Bundle {
    // for commit 
    val cmt_en          = Input(Vec(2, Bool()))
    val prd_cmt         = Input(Vec(2, UInt(log2Ceil(n).W)))
    val pprd_cmt        = Input(Vec(2, UInt(log2Ceil(n).W)))
    val rd_valid_cmt    = Input(Vec(2, Bool()))
    val predict_fail    = Input(Bool())

    // for reg rename
    val arch_rat        = Output(Vec(n, UInt(1.W)))
    val head_arch       = Output(UInt(log2Ceil(n).W))

    // for ras
    val top_arch            = Output(UInt(3.W))
    val br_type_pred_cmt    = Input(UInt(2.W))
    val pc_cmt              = Input(UInt(32.W))
    val pred_update_en_cmt  = Input(Bool())
    val ras_arch            = Output(Vec(8, UInt(32.W)))
}

class Arch_Rat(n: Int) extends Module {
    val io = IO(new Arch_Rat_IO(n))

    val arat = RegInit(VecInit.fill(n)(false.B))

    val head = RegInit(0.U(log2Ceil(n).W))
    var head_next = head
    for(i <- 0 until 2){
        when(io.rd_valid_cmt(i) && io.cmt_en(i)){
            head_next := Mux(head_next === (n-1).U, 0.U, head_next + 1.U)
            arat(io.pprd_cmt(i)) := false.B
            arat(io.prd_cmt(i)) := true.B
        }
    }
    head := head_next
    
    io.arch_rat     := arat
    io.head_arch    := head


    // ras
    val top         = RegInit(0x7.U(3.W))
    val ras         = RegInit(VecInit.fill(8)(0x1c000000.U(32.W)))
    val stack_pop  = io.br_type_pred_cmt === RET && io.pred_update_en_cmt
    val stack_push = io.br_type_pred_cmt(1) && io.pred_update_en_cmt
    when(stack_pop){
        top := top - 1.U
    }.elsewhen(stack_push){
        top := top + 1.U
        ras(top) := io.pc_cmt
    }
    
    io.top_arch := top
    io.ras_arch := ras

}
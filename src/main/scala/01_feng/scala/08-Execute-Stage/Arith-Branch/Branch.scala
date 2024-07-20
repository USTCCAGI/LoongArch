import chisel3._
import chisel3.util._
import Control_Signal._

class Branch_IO extends Bundle {
    val br_type         = Input(UInt(4.W))  //0:beq 1:bne 2:blt 3:bge 4:bltu 5:bgeu 6:jirl 7:b 8:bl
    val src1            = Input(UInt(32.W)) //指令两操作数
    val src2            = Input(UInt(32.W)) //指令两操作数
    val pc_ex           = Input(UInt(32.W)) //指令内容
    val imm_ex          = Input(UInt(32.W)) //指令内容
    val predict_jump    = Input(Bool())     //输入预测是否跳转
    val pred_npc        = Input(UInt(32.W)) //输入预测结果
    val real_jump       = Output(Bool())    //真实是否跳转
    val predict_fail    = Output(Bool())    //预测是否失败信号
    val branch_target   = Output(UInt(32.W)) //跳转目标
}

class Branch extends Module {
    val io = IO(new Branch_IO)
    io.predict_fail     := false.B
    io.branch_target    := io.pc_ex + io.imm_ex
    io.real_jump        := false.B
    switch(io.br_type) {
        is(BR_BEQ){
            io.predict_fail     := io.real_jump ^ io.predict_jump || (io.predict_jump && (io.pred_npc ^ io.branch_target).orR)
            io.real_jump        := (io.src1 === io.src2)
        }
        is(BR_BNE){
            io.predict_fail     := io.real_jump ^ io.predict_jump || (io.predict_jump && (io.pred_npc ^ io.branch_target).orR)
            io.real_jump        := (io.src1 =/= io.src2)
        }
        is(BR_BLT){
            io.predict_fail     := io.real_jump ^ io.predict_jump || (io.predict_jump && (io.pred_npc ^ io.branch_target).orR)
            io.real_jump        := (io.src1.asSInt < io.src2.asSInt)
        }
        is(BR_BGE){
            io.predict_fail     := io.real_jump ^ io.predict_jump || (io.predict_jump && (io.pred_npc ^ io.branch_target).orR)
            io.real_jump        := (io.src1.asSInt >= io.src2.asSInt)
        }
        is(BR_BLTU){
            io.predict_fail     := io.real_jump ^ io.predict_jump || (io.predict_jump && (io.pred_npc ^ io.branch_target).orR)
            io.real_jump        := (io.src1 < io.src2)
        }
        is(BR_BGEU){
            io.predict_fail     := io.real_jump ^ io.predict_jump || (io.predict_jump && (io.pred_npc ^ io.branch_target).orR)
            io.real_jump        := (io.src1 >= io.src2)
        }
        is(BR_JIRL){
            io.predict_fail     := ~io.predict_jump || ((io.pred_npc ^ io.branch_target).orR)
            io.branch_target    := io.src1 + io.imm_ex
            io.real_jump        := true.B
        }
        is(BR_B){
            io.predict_fail     := ~io.predict_jump || ((io.pred_npc ^ io.branch_target).orR)
            io.real_jump        := true.B
        }
        is(BR_BL){
            io.predict_fail     := ~io.predict_jump || ((io.pred_npc ^ io.branch_target).orR)
            io.real_jump        := true.B
        }
    }
}

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

class Branch extends Module{
    val io=IO(new Branch_IO)
    io.predict_fail     := false.B
    io.real_jump        := false.B
    io.branch_target    := io.pc_ex + io.imm_ex

    switch(io.br_type){
        is(BR_BEQ){//beq pc=pc+imm
            when(io.src1 === io.src2){
                io.real_jump := true.B
            }
        }
        is(BR_BNE){
            when(io.src1 =/= io.src2){
                io.real_jump := true.B
            }
        }
        is(BR_BLT){
            when(io.src1.asSInt < io.src2.asSInt){
                io.real_jump := true.B
            }
        }
        is(BR_BGE){
            when(io.src1.asSInt >= io.src2.asSInt){
                io.real_jump := true.B
            }
        }
        is(BR_BLTU){
            when(io.src1 < io.src2){
                io.real_jump := true.B
            }
        }
        is(BR_BGEU){
            when(io.src1 >= io.src2){
                io.real_jump := true.B
            }
        }
        is(BR_JIRL){
            io.real_jump := true.B   //无条件跳转
            io.branch_target := io.src1 + io.imm_ex
        }
        is(BR_B){
            io.real_jump := true.B
        }
        is(BR_BL){
            io.real_jump := true.B
        }
    }
    when(io.predict_jump =/= io.real_jump|| (io.predict_jump && (io.pred_npc =/= io.branch_target))){
        io.predict_fail := true.B
    }
}
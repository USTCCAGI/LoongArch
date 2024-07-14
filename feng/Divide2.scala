import chisel3._
import chisel3.util._
import Control_Signal._

class Divide2 extends Module{
    val io=IO(new Bundle{
        val src1=Input(UInt(32.W))
        val src2=Input(UInt(32.W))
        val op  =Input(UInt(5.W))
        val res =Output(UInt(32.W))
        val busy=Output(Vec(32, Bool()))//忙等线0123
    })

    //第一段： 记录src2和op和sign，符号修正获得绝对值
    val en = io.op(2)
    val sign = Wire(UInt(1.W))

    when(io.op === ALU_DIV){
        sign := io.src1(31) ^ io.src2(31)
    }.elsewhen(io.op === ALU_MOD){
        sign := io.src1(31)
    }.otherwise{
        sign := 0.U
    }

    val src1 = Wire(UInt(32.W))
    val src2 = Wire(UInt(32.W))
    when((io.op === ALU_DIV || io.op === ALU_MOD) && io.src1(31)){
        src1 := ~io.src1 + 1.U
    }.otherwise{
        src1 := io.src1
    }
    when((io.op === ALU_DIV || io.op === ALU_MOD) && io.src2(31)){
        src2 := ~io.src2 + 1.U
    }.otherwise{
        src2 := io.src2
    }

    //获取src1中最高位的1，算一次除法需要多少个周期

    val high = PriorityEncoder(Reverse(src1))
    val cnt = RegInit(VecInit.fill(32)(0.U(6.W)))
    val en_reg1 = ShiftRegister(en, 1, !io.busy(3))
    val high_reg1 = ShiftRegister(high, 1, !io.busy(3))
    for(i <- 0 until 32){
        when(cnt(i) =/= 0.U){
            cnt(i) := cnt(i) - 1.U
        }.elsewhen(en_reg1){
            cnt(i) := 33.U - high_reg1
        }
    }

    //多段流水
    val src1_reg1 = ShiftRegister(src1, 1, !io.busy(0))
    val src2_reg1 = ShiftRegister(src2, 1, !io.busy(1))

    val op_reg1 = ShiftRegister(io.op, 1, !io.busy(2))
    val sign_reg1 = ShiftRegister(sign, 1, !io.busy(2))

    val src2_reg2 = RegInit(0.U(32.W))
    val op_reg2 = RegInit(0.U(5.W))
    val sign_reg2 = RegInit(0.U)

    //第二段输出，计算结果
    when(en_reg1 && cnt(1) === 0.U){
        op_reg2 := op_reg1
        src2_reg2 := src2_reg1
        sign_reg2 := sign_reg1
    }

    io.busy := cnt.map(_ =/= 0.U)
    
    //第三段： 除法计算
    val q_r = RegInit(0.U(65.W))
    when(cnt(1) =/= 0.U){
        when(q_r(63,32)>=src2_reg2){
            q_r := (q_r(63,32)-src2_reg2) ## q_r(31,0) ## 1.U(1.W)
        }.otherwise{
            q_r := (q_r(63,0) ## 0.U(1.W))
        }
    }.elsewhen(en_reg1){//除法初始化，被除数绝对值放到RQ低32位
        q_r := 0.U(33.W) ## src1_reg1 << high_reg1
    } 

    val res = Wire(UInt(32.W))
    when(op_reg2 === ALU_DIV){
        when(sign_reg2 === 1.U){
            res := ~q_r(31,0) + 1.U
        }.otherwise{
            res := q_r(31,0)
        }
    }.elsewhen(op_reg2 === ALU_MOD){
        when(sign_reg2 ===  1.U){
            res := ~q_r(64,33) + 1.U
        }.otherwise{
            res := q_r(64,33)
        }
    }.elsewhen(op_reg2 === ALU_DIVU){
        res := q_r(31,0)
    }.otherwise{
        res := q_r(64,33)
    }

    io.res := res

}
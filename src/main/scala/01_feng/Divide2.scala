import chisel3._
import chisel3.util._
import Control_Signal._

class Divide extends Module{
    val io=IO(new Bundle{
        val num1=Input(UInt(32.W))
        val num2=Input(UInt(32.W))
        val op  =Input(UInt(5.W))
        val div_out =Output(UInt(32.W))
        val busy=Output(Vec(32, Bool()))//忙等线0123
    })

    //第一段： 记录num2和op和sign，符号修正获得绝对值
    val en = io.op(2)
    val sign = Wire(UInt(1.W))

    when(io.op === ALU_DIV){
        sign := io.num1(31) ^ io.num2(31)
    }.elsewhen(io.op === ALU_MOD){
        sign := io.num1(31)
    }.otherwise{
        sign := false.B
    }

    val num1 = Wire(UInt(32.W))
    val num2 = Wire(UInt(32.W))
    when((io.op === ALU_DIV || io.op === ALU_MOD) && io.num1(31)){
         num1 := ~io.num1 + 1.U
    }.otherwise{
        num1 := io.num1
    }
    when((io.op === ALU_DIV || io.op === ALU_MOD) && io.num2(31)){
        num2 := ~io.num2 + 1.U
    }.otherwise{
        num2 := io.num2
    }

    //获取num1中最高位的1，算一次除法需要多少个周期

    val high = PriorityEncoder(Reverse(num1))
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
    val num1_reg1 = ShiftRegister(num1, 1, !io.busy(0))
    val num2_reg1 = ShiftRegister(num2, 1, !io.busy(1))

    val op_reg1 = ShiftRegister(io.op, 1, !io.busy(2))
    val sign_reg1 = ShiftRegister(sign, 1, !io.busy(2))

    val num2_reg2 = RegInit(0.U(32.W))
    val op_reg2 = RegInit(0.U(5.W))
    val sign_reg2 = RegInit(0.U)

    //第二段输出，计算结果
    when(en_reg1 && cnt(1) === 0.U){
        op_reg2 := op_reg1
        num2_reg2 := num2_reg1
        sign_reg2 := sign_reg1
    }

    io.busy := cnt.map(_ =/= 0.U)
    
    //第三段： 除法计算
    val q_r = RegInit(0.U(65.W))
    when(cnt(1) =/= 0.U){
        when(q_r(63,32)>=num2_reg2){
            q_r := (q_r(63,32)-num2_reg2) ## q_r(31,0) ## 1.U(1.W)
        }.otherwise{
            q_r := (q_r(63,0) ## 0.U(1.W))
        }
    }.elsewhen(en_reg1){//除法初始化，被除数绝对值放到RQ低32位
        q_r := (0.U(33.W) ## num1_reg1 )<< high_reg1
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

    io.div_out := res

}
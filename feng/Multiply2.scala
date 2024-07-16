import chisel3._
import chisel3.util._
import Control_Signal._

//feng
class Multiply2 extends Module{
    val io = IO(new Bundle{
        val num1 = Input(UInt(32.W))
        val num2 = Input(UInt(32.W))
        val op   = Input(UInt(5.W))
        val mul_out  = Output(UInt(32.W))
        val busy = Input(Vec(32, Bool()))//来自div
        
    })

    val num1 = Wire(UInt(66.W))
    val num2 = Wire(UInt(66.W))
    num1 := 0.U
    num2 := 0.U

    when(io.op === ALU_MULHU){
        num1 := 0.U(34.W) ## io.num1
        num2 := 0.U(34.W) ## io.num2
    }.otherwise{
        num1 := Fill(34, io.num1(31)) ## io.num1
        num2 := Fill(34, io.num2(31)) ## io.num2
    }

    //Booth编码，使用乘数将被乘数编码
    val Num_x = Wire(Vec(33, UInt(66.W)))//x
    val booth  = Wire(Vec(33, UInt(3.W)))
    booth(0)  := num2(1) ## num2(0) ##  0.U // 最低位B(-1)=0
    for(i<-1 until 33){
        booth(i) := num2(2*i+1) ## num2(2*i) ## num2(2*i-1)
    }

    for (i <- 0 until 33) {
        val temp = Cat(num1(65-2*i, 0),0.U((2*i).W))
        when(booth(i)===1.U || booth(i)===2.U){
            Num_x(i) := temp
        }.elsewhen(booth(i)===3.U){
            Num_x(i) := ((temp)<<1)
        }.elsewhen(booth(i)===4.U){
            Num_x(i) := (~(temp<<1)+1.U)
        }.elsewhen(booth(i)===5.U || booth(i)===6.U){
            Num_x(i) := (~(temp)+1.U)
        }.otherwise{
            Num_x(i) := 0.U
        }

    }
    //第一寄存器
   
    val Num_reg1 = ShiftRegister(Num_x, 1, !io.busy(6))
    //保留进位加法器
    def CSA(x: UInt, y: UInt, z: UInt) : UInt  = {
        val mul_out1 = (x ^ y) ^ z
        val mul_out2 = ((((x & y) | z & (x ^ y)) << 1)(65, 0))
        mul_out2 ## mul_out1
    }

    //华莱士树
    //一级：33个输入(Num_reg1)，22个输出,每3个booth分配一个保留进位加法器
    val input_1 =  Wire(Vec(11, UInt(132.W)))
    val output_1 = Wire(Vec(22,UInt(66.W)))
    for(i<-0 until 11){
        input_1(i) := CSA(Num_reg1(3*i), Num_reg1(3*i+1), Num_reg1(3*i+2))
        output_1(2*i) := input_1(i)(65,0)
        output_1(2*i+1) := input_1(i)(131,66)
    }
    //二级，22个输入，15个输出,多余1条线(21_input)
    val input_2 =  Wire(Vec(7, UInt(132.W)))
    val output_2 = Wire(Vec(15, UInt(66.W)))
    for(i<-0 until 7){
        input_2(i) := CSA(output_1(3*i), output_1(3*i+1), output_1(3*i+2))
        output_2(2*i) := input_2(i)(65,0)
        output_2(2*i+1) := input_2(i)(131,66)
    }
    output_2(14) := output_1(21)
    //三级，15个输入，10个输出
    val input_3 =  Wire(Vec(5, UInt(132.W)))
    val output_3 = Wire(Vec(10, UInt(66.W)))
    for(i<-0 until 5){
        input_3(i) := CSA(output_2(3*i), output_2(3*i+1), output_2(3*i+2))
        output_3(2*i) := input_3(i)(65,0)
        output_3(2*i+1) := input_3(i)(131,66)
    }
    //四级，10个输入，7个输出
    val input_4 =  Wire(Vec(3, UInt(132.W)))
    val output_4 = Wire(Vec(7, UInt(66.W)))
    for(i<-0 until 3){
        input_4(i) := CSA(output_3(3*i), output_3(3*i+1), output_3(3*i+2))
        output_4(2*i) := input_4(i)(65,0)
        output_4(2*i+1) := input_4(i)(131,66)
    }
    output_4(6) := output_3(9)
    //五级，7个输入，5个输出
    val input_5 =  Wire(Vec(2, UInt(132.W)))
    val output_5 = Wire(Vec(5, UInt(66.W)))
    for(i<-0 until 2){
        input_5(i) := CSA(output_4(3*i), output_4(3*i+1), output_4(3*i+2))
        output_5(2*i) := input_5(i)(65,0)
        output_5(2*i+1) := input_5(i)(131,66)
    }
    output_5(4) := output_4(6)
    //六级，5个输入，4个输出
    val input_6 =  Wire(Vec(1, UInt(132.W)))
    val output_6 = Wire(Vec(4, UInt(66.W)))
    for(i<-0 until 1){
        input_6(i) := CSA(output_5(3*i), output_5(3*i+1), output_5(3*i+2))
        output_6(2*i) := input_6(i)(65,0)
        output_6(2*i+1) := input_6(i)(131,66)
    }
    output_6(2) := output_5(3)
    output_6(3) := output_5(4)
    //七级，4个输入，3个输出
    val input_7 =  Wire(Vec(1, UInt(132.W)))
    val output_7 = Wire(Vec(3, UInt(66.W)))
    for(i<-0 until 1){
        input_7(i) := CSA(output_6(3*i), output_6(3*i+1), output_6(3*i+2))
        output_7(2*i) := input_7(i)(65,0)
        output_7(2*i+1) := input_7(i)(131,66)
    }
    output_7(2) := output_6(3)
    //八级，3个输入，2个输出
    val input_8 =  Wire(Vec(1, UInt(132.W)))
    val output_8 = Wire(Vec(2, UInt(66.W)))
    for(i<-0 until 1){
        input_8(i) := CSA(output_7(3*i), output_7(3*i+1), output_7(3*i+2))
        output_8(2*i) := input_8(i)(65,0)
        output_8(2*i+1) := input_8(i)(131,66)
    }
    //输出
    //三段流水输出
    val mul_out1 = ShiftRegister(output_8(0)(63, 0), 1, !io.busy(7))
    val mul_out2 = ShiftRegister(output_8(1)(63, 0), 1, !io.busy(8))
    val op_reg1      = ShiftRegister(io.op, 1, !io.busy(9))
    val op_reg2      = ShiftRegister(op_reg1, 1, !io.busy(10))
    val mul_out = mul_out1 + mul_out2

    when(op_reg2  === ALU_MUL){
        io.mul_out := mul_out(31,0)
    }.otherwise{
        io.mul_out := mul_out(63,32)
    }
}

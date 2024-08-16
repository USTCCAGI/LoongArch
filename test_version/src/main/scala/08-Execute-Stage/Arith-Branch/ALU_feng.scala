import chisel3._ 
import chisel3.util._
import Control_Signal._
import scala.collection.IndexedSeqView.Reverse

class ALU_IO extends Bundle{
    val src1        = Input(UInt(32.W))
    val src2        = Input(UInt(32.W))
    val imm_ror     = Input(UInt(32.W))
    val alu_op      = Input(UInt(4.W))
    val alu_out     = Output(UInt(32.W))
} 

class ALU extends Module {
    val io = IO(new ALU_IO)

    val src1 = io.src1
    val src2 = io.src2

    val alu_op = io.alu_op

    val result = Wire(UInt(32.W))

    result := 0.U(32.W)

    switch(alu_op){
        is(ALU_ADD){//1
            result := src1 + src2
        }
        is(ALU_SUB){//2
            result := src1 - src2
        }
        is(ALU_AND){//3
            result := src1 & src2
        }
        is(ALU_OR){//4
            result := src1 | src2
        }
        is(ALU_XOR){//5
            result := src1 ^ src2
        }
        is(ALU_SLT){//6
            result := (src1.asSInt < src2.asSInt).asUInt
        }
        is(ALU_SLTU){//7
            result := (src1 < src2).asUInt
        }
        is(ALU_SLL){//8
            result := src1 << (src2(4,0))
        }
        is(ALU_SRA){//9
            result := (src1.asSInt >> src2(4, 0)).asUInt
        }
        is(ALU_SRL){//10
            result := src1 >> (src2(4,0))
        }
        is(ALU_NOR){//11
            result := ~(src1 | src2)
        }
        is(ALU_ROR){
            val imm_ror = io.imm_ror
            val data0 = src1(31, 16)
            val data1 = src1(15, 0)
            val data2 = src2(31, 16)
            val data3 = src2(15, 0)

            // count 0
            val num0 = PriorityEncoder(Reverse(data0))
            val num1 = PriorityEncoder(Reverse(data1))
            val num2 = PriorityEncoder(Reverse(data2))
            val num3 = PriorityEncoder(Reverse(data3))

            //find max
            val max_num1 = Mux(num0 > num1, num0, num1)
            val max_num2 = Mux(num2 > num3, num2, num3)
            val max_num = Mux(max_num1 > max_num2, max_num1, max_num2)

            result := (imm_ror >> max_num) | (imm_ror << (32.U - max_num))
        }
    }

    io.alu_out := result
    
}

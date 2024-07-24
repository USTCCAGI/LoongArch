import chisel3._ 
import chisel3.util._
import Control_Signal._

class ALU_IO extends Bundle{
    val src1        = Input(UInt(32.W))
    val src2        = Input(UInt(32.W))
    val alu_op      = Input(UInt(4.W))
    val alu_out     = Output(UInt(32.W))
} 

class ALU extends Module {
    val io = IO(new ALU_IO)

    val src1 = io.src1
    val src2 = io.src2

    val alu_op = io.alu_op

    val result = Wire(UInt(32.W))

    result := DontCare

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
            result := src1 >> (src2(4,0))
        }
        is(ALU_SRL){//10
            result := src1 >> (src2(4,0))
        }
        is(ALU_NOR){//11
            result := ~(src1 | src2)
        }
    }

    io.alu_out := result
    
}

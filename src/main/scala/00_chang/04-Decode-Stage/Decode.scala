import chisel3._
import chisel3.util._
import Decode_Map._

class DecodeIO extends Bundle{
    val inst            = Input(UInt(32.W))
    
    val rj              = Output(UInt(5.W))
    val rk              = Output(UInt(5.W))
    val rd              = Output(UInt(5.W))
    val rd_valid        = Output(Bool())

    val imm             = Output(UInt(32.W))
    val alu_op          = Output(UInt(5.W))
    val alu_rs1_sel     = Output(UInt(1.W))
    val alu_rs2_sel     = Output(UInt(2.W))
    val br_type         = Output(UInt(4.W))
    val mem_type        = Output(UInt(5.W))

    val priv_vec        = Output(UInt(13.W))

    val fu_id           = Output(UInt(3.W))
    val exception       = Output(UInt(8.W))
}
class Decode extends Module{
    val io = IO(new DecodeIO)
    val signal = ListLookup(io.inst, Decode_Map.default, Decode_Map.map)

    io.rj               := Mux(signal(0).asBool, io.inst(9, 5), 0.U(5.W))

    io.rk               := Mux(signal(1).asBool, Mux(signal(9)(0).asBool, io.inst(14, 10), io.inst(4, 0)), 0.U)

    io.rd               := MuxLookup(signal(10), io.inst(4, 0))(Seq(
                            Control_Signal.RD    -> io.inst(4, 0),
                            Control_Signal.R1    -> 1.U(5.W),
                            Control_Signal.RJ    -> io.inst(9, 5)))
    io.rd_valid         := signal(2) & io.rd =/= 0.U(5.W)

    io.alu_op           := signal(3)
    io.alu_rs1_sel      := signal(4)
    io.alu_rs2_sel      := signal(5)

    io.br_type          := signal(6)
    io.mem_type         := signal(7)

    io.priv_vec         := signal(12)
    
    io.fu_id            := signal(8)
    io.exception        := Mux(signal(12)(8) && io.inst(4, 0) >= 7.U, 1.U ## Control_Signal.INE, signal(13))


    def Imm_Gen(inst: UInt, imm_type: UInt): UInt = {
        val imm = Wire(UInt(32.W))
        import Control_Signal._
        imm := DontCare
        switch(imm_type) {
            is(IMM_00U)     { imm := 0.U(32.W) }
            is(IMM_05U)     { imm := Cat(0.U(27.W), inst(14, 10)) }
            is(IMM_12U)     { imm := Cat(0.U(20.W), inst(21, 10)) }
            is(IMM_12S)     { imm := Cat(Fill(20, inst(21)), inst(21, 10)) }
            is(IMM_14S)     { imm := Cat(Fill(18, inst(21)), inst(21, 10), 0.U(2.W)) }
            is(IMM_16S)     { imm := Cat(Fill(14, inst(25)), inst(25, 10), 0.U(2.W)) }
            is(IMM_20S)     { imm := Cat(inst(24, 5), 0.U(12.W)) }
            is(IMM_26S)     { imm := Cat(Fill(4, inst(9)), inst(9, 0), inst(25, 10), 0.U(2.W)) }
            is(IMM_CSR)     { imm := Cat(0.U(18.W), inst(23, 10)) }
            is(IMM_TID)     { imm := 0x40.U(32.W) }
            is(IMM_ERA)     { imm := 0x6.U(32.W) }
            is(IMM_COP)     { imm := Cat(Fill(15, inst(21)), inst(21, 10), inst(4, 0))}
        }
        imm
    }

    io.imm              := Imm_Gen(io.inst, signal(11))
}




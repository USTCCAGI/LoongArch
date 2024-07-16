import chisel3._
import chisel3.util._

object Predict_Config {
    val BTB_INDEX_WIDTH = 7
    val BTB_TAG_WIDTH   = 28 - BTB_INDEX_WIDTH
    val BTB_DEPTH       = 1 << BTB_INDEX_WIDTH
    val BHT_INDEX_WIDTH = 6
    val BHT_DEPTH       = 1 << BHT_INDEX_WIDTH
    val PHT_INDEX_WIDTH = 6
    val PHT_DEPTH       = 1 << PHT_INDEX_WIDTH

    val RET       = 1.U(2.W)
    val BL        = 2.U(2.W)
    val ICALL     = 3.U(2.W)
    val ELSE      = 0.U(2.W)
}

object Predict_Struct{
    class btb_t extends Bundle{
        val valid       = Bool()
        val target      = UInt(30.W)
        val tag         = UInt(BTB_TAG_WIDTH.W)
        val typ         = UInt(2.W)
    }
}

class Predict_IO extends Bundle{
    // check
    val npc                 = Input(Vec(10, UInt(32.W)))
    val pc                  = Input(Vec(10, UInt(32.W)))
    val predict_jump        = Output(Vec(2, Bool()))
    val pred_npc            = Output(UInt(32.W))
    val pred_valid          = Output(Vec(2, Bool()))
    val pc_stall            = Input(Bool())

    // update   
    val pc_cmt              = Input(UInt(32.W))
    val real_jump           = Input(Bool())
    val branch_target       = Input(UInt(32.W))
    val update_en           = Input(Bool())
    val br_type             = Input(UInt(2.W))

    // recover 
    val top_arch            = Input(UInt(3.W))
    val ras_arch            = Input(Vec(8, UInt(32.W)))
    val predict_fail        = Input(Bool())
    val pd_pred_fix         = Input(Bool())
    val pd_pred_fix_is_bl   = Input(Bool())
    val pd_pc_plus_4        = Input(UInt(32.W))
    
}

class Predict extends Module{
    
    val io = IO(new Predict_IO)
    
    val btb_tagv    = VecInit.fill(2)(Module(new xilinx_simple_dual_port_1_clock_ram_read_first(BTB_TAG_WIDTH+1, BTB_DEPTH)).io)
    val btb_targ    = VecInit.fill(2)(Module(new xilinx_simple_dual_port_1_clock_ram_read_first(30+2, BTB_DEPTH)).io)

    


}
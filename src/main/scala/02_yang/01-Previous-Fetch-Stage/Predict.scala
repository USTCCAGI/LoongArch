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

// BHB: Tag-[31, 10], Index-[9, 3], Offset-2

class Predict extends Module{
    
    val io = IO(new Predict_IO)
    
    val btb_tagv    = VecInit.fill(2)(VecInit.fill(2)(Module(new xilinx_simple_dual_port_1_clock_ram_read_first(BTB_TAG_WIDTH+1, BTB_DEPTH)).io))
    val btb_targ    = VecInit.fill(2)(VecInit.fill(2)(Module(new xilinx_simple_dual_port_1_clock_ram_read_first(30+2, BTB_DEPTH)).io))
    val bht         = RegInit(VecInit.fill(2)(VecInit.fill(BHT_DEPTH)(0.U(4.W))))

    val lpht       = RegInit(VecInit.fill(2)(VecInit.fill(PHT_DEPTH)(2.U(2.W))))

    val gpht       = RegInit(VecInit.fill(2)(VecInit.fill(PHT_DEPTH)(2.U(2.W))))
    val ghr         = RegInit(0.U(8.W))
    val ras         = RegInit(VecInit.fill(8)(0x1c000000.U(32.W)))
    val jirl_sel    = RegInit(2.U(2.W))
    val top         = RegInit(0x7.U(3.W))

    // check
    val npc             = io.npc
    val pc              = io.pc
    val pc_cmt          = io.pc_cmt
    val cmt_col         = pc_cmt(2)
    
    val btb_rindex      = VecInit.tabulate(2)(i => npc(i)(3+BTB_INDEX_WIDTH-1, 3))
    val btb_rdata       = Wire(VecInit(2)(Vec(2, new btb_t)))

    val bht_rindex      = VecInit.tabulate(2)(i => pc(i)(3+BHT_INDEX_WIDTH-1, 3))
    val bht_rdata       = VecInit.tabulate(2)(i => bht(i)(bht_rindex(i)))

    val lpht_rindex      = VecInit.tabulate(2)(i => bht_rdata(i)(3, 2)  ## (bht_rdata(i)(1, 0) ^ pc(i+2)(PHT_INDEX_WIDTH, PHT_INDEX_WIDTH-1)) ## pc(i+2)(PHT_INDEX_WIDTH-2, 3))
    val lpht_rdata       = VecInit.tabulate(2)(i => lpht(i)(lpht_rindex(i)))

    val predict_valid   = VecInit.tabulate(2)(i => btb_rdata(btb_rsel)(i).valid && !(btb_rdata(btb_rsel)(i).tag ^ pc(i+4)(31, 32 - BTB_TAG_WIDTH)))
    val predict_jump    = VecInit.tabulate(2)(i => (lpht_rdata(i)(1)) && predict_valid(i))

    val valid_mask      = true.B ## !pc(6)(2)
    val pred_hit        = VecInit.tabulate(2)(i => predict_jump(i) && valid_mask(i))
    val pred_valid_hit  = VecInit.tabulate(2)(i => predict_valid(i) && valid_mask(i))

    val pred_hit_index  = !pred_hit(0)

    io.predict_jump     := (pred_hit(1) ## Mux(pc(6)(2), pred_hit(1), pred_hit(0))).asBools
    io.pred_valid       := (pred_valid_hit(1) ## Mux(pc(6)(2), pred_valid_hit(1), pred_valid_hit(0))).asBools
    io.pred_npc         := Mux(btb_rdata(btb_rsel)(pred_hit_index).typ === RET, ras(top-1.U) + 4.U, btb_rdata(btb_rsel)(pred_hit_index).target ## 0.U(2.W)) 
    
    // update
    val update_en       = io.update_en
    // btb
    val mask            = UIntToOH(cmt_col)
    val btb_wdata       = Wire(Vec(2, new btb_t))
    val btb_windex      = pc_cmt(3-1+BTB_INDEX_WIDTH, 3)

    // way_sel reg
    val way_sel         = RegInit(VecInit(BTB_DEPTH)(0.U(1.W)))
    when (update_en){
        way_sel(btb_windex) := ~way_sel(btb_windex)
    }

    // btb_rsel
    val btb_rsel        = Mux((btb_rdata(1)(0).tag ^ pc(4)(31, 32 - BTB_TAG_WIDTH)) || (btb_rdata(1)(1).tag ^ pc(1+4)(31, 32 - BTB_TAG_WIDTH)), 1.U(1.W), 0.U(1.W))

    for (i <- 0 until 2){
        btb_wdata(i).valid  := true.B
        btb_wdata(i).target := io.branch_target(31, 2)
        btb_wdata(i).tag    := pc_cmt(31, 32-BTB_TAG_WIDTH)
        btb_wdata(i).typ    := io.br_type
    }
    for(i <- 0 until 2)
        for(j <- 0 until 2){
            btb_tagv(i)(j).addra   := btb_windex
            btb_tagv(i)(j).addrb   := btb_rindex(j)
            btb_tagv(i)(j).dina    := btb_wdata(j).valid ## btb_wdata(j).tag
            btb_tagv(i)(j).clka    := clock
            btb_tagv(i)(j).wea     := update_en && mask(j) && (way_sel(btb_windex) == i)
    }
    for(i <- 0 until 2)
        for(j <- 0 until 2){
            btb_rdata(i)(j).valid  := btb_tagv(i)(j).doutb(BTB_TAG_WIDTH)
            btb_rdata(i)(j).tag    := btb_tagv(i)(j).doutb(BTB_TAG_WIDTH-1, 0)
            btb_rdata(i)(j).target := btb_targ(i)(j).doutb(31, 2)
            btb_rdata(i)(j).typ    := btb_targ(i)(j).doutb(1, 0)
    }
    for(i <- 0 until 2)
        for(j <- 0 until 2){
            btb_targ(i)(j).addra   := btb_windex
            btb_targ(i)(j).addrb   := btb_rindex(j)
            btb_targ(i)(j).dina    := btb_wdata(j).target ## btb_wdata(j).typ
            btb_targ(i)(j).clka    := clock
            btb_targ(i)(j).wea     := update_en && mask(j) && (way_sel == i)
    }
    // bht
    val bht_windex = pc_cmt(3-1+BHT_INDEX_WIDTH, 3)
    val bht_wdata  = io.real_jump
    when(update_en){
        bht(cmt_col)(bht_windex) := bht_wdata ## bht(cmt_col)(bht_windex)(3, 1)
    }

    // local pht
    val lpht_windex = bht(cmt_col)(bht_windex)(3, 2) ## (bht(cmt_col)(bht_windex)(1, 0) ^ pc_cmt(PHT_INDEX_WIDTH, PHT_INDEX_WIDTH-1)) ## pc_cmt(PHT_INDEX_WIDTH-2, 3)
    val lpht_raw_rdata = lpht(cmt_col)(lpht_windex)

    when(update_en){
        lpht(cmt_col)(lpht_windex) := Mux(io.real_jump, 
                                        lpht_raw_rdata + (lpht_raw_rdata =/= 3.U), 
                                        lpht_raw_rdata - (lpht_raw_rdata =/= 0.U))
    }

    // global pht

    val gpht_rindex      = VecInit.tabulate(2)(i => ghr(3, 2)  ## (ghr ^ pc(i+2)(PHT_INDEX_WIDTH, PHT_INDEX_WIDTH-1)) ## pc(i+2)(PHT_INDEX_WIDTH-2, 3))
    val gpht_rdata       = VecInit.tabulate(2)(i => gpht(i)(gpht_rindex(i)))
    val gpht_windex      = ghr(3, 2) ## (ghr(1, 0) ^ pc_cmt(PHT_INDEX_WIDTH, PHT_INDEX_WIDTH-1)) ## pc_cmt(PHT_INDEX_WIDTH-2, 3)
    val gpht_raw_rdata   = gpht(cmt_col)(gpht_windex)

    when(update_en){
        gpht(cmt_col)(gpht_windex) := Mux(io.real_jump, 
                                        gpht_raw_rdata + (gpht_raw_rdata =/= 3.U), 
                                        gpht_raw_rdata - (gpht_raw_rdata =/= 0.U))
        ghr     :=  ghr(3:1) ## Mux(io.real_jump, 1.U, 0.U)
    }

    // Competition
    

    // RAS
    when(io.predict_fail){
        top := io.top_arch
        ras := io.ras_arch
    }
    .elsewhen(io.pd_pred_fix){
        when(io.pd_pred_fix_is_bl){
            top             := top + 1.U
            ras(top + 1.U)  := io.pd_pc_plus_4
        }
    }.elsewhen(btb_rdata(pred_hit_index).typ(1) && pred_valid_hit(pred_hit_index)){
        top                 := top + 1.U
        ras(top + 1.U)      := pc(6)(31, 3) ## pred_hit_index ## 0.U(2.W)
    }.elsewhen(btb_rdata(pred_hit_index).typ === RET && pred_valid_hit(pred_hit_index)){
        top             := top - 1.U
    }

    when(io.update_en && io.br_type === RET){
        jirl_sel := Mux(io.predict_fail, Mux(jirl_sel(0), 2.U, 1.U), Mux(jirl_sel(1), 3.U, 0.U))
    }

}
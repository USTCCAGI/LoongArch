import chisel3._
import chisel3.util._
import Predict_Config._
import CPU_Config._
import TLB_Struct._
import ROB_Struct._

class ROB_IO(n: Int) extends Bundle{
    // for reg rename
    val inst_valid_dp           = Input(Vec(2, Bool()))
    val rd_dp                   = Input(Vec(2, UInt(5.W)))
    val rd_valid_dp             = Input(Vec(2, Bool()))
    val prd_dp                  = Input(Vec(2, UInt(log2Ceil(PREG_NUM).W)))
    val pprd_dp                 = Input(Vec(2, UInt(log2Ceil(PREG_NUM).W)))
    val rob_index_dp            = Output(Vec(2, UInt(log2Ceil(n).W)))
    val pc_dp                   = Input(Vec(2, UInt(32.W)))
    val is_store_dp             = Input(Vec(2, Bool()))
    val br_type_pred_dp         = Input(Vec(2, UInt(2.W)))
    val pred_update_en_dp       = Input(Vec(2, Bool()))
    val priv_vec_dp             = Input(Vec(2, UInt(13.W)))
    val exception_dp            = Input(Vec(2, UInt(8.W)))
    val inst_dp                 = Input(Vec(2, UInt(32.W)))
    val full                    = Output(Vec(10, Bool()))
    val stall                   = Input(Bool())

    // for wb stage 
    val inst_valid_wb           = Input(Vec(4, Bool()))
    val rob_index_wb            = Input(Vec(4, UInt(log2Ceil(n).W)))
    val exception_wb            = Input(Vec(4, UInt(8.W)))
    val is_ucread_wb            = Input(Vec(4, Bool()))
    val predict_fail_wb         = Input(Vec(4, Bool()))
    val real_jump_wb            = Input(Vec(4, Bool()))
    val branch_target_wb        = Input(Vec(4, UInt(32.W)))
    val rf_wdata_wb             = Input(Vec(4, UInt(32.W)))

    // for cpu state: arch rat
    val cmt_en                  = Output(Vec(2, Bool()))
    val prd_cmt                 = Output(Vec(2, UInt(log2Ceil(PREG_NUM).W)))
    val rd_valid_cmt            = Output(Vec(2, Bool()))
    val pprd_cmt                = Output(Vec(2, UInt(log2Ceil(PREG_NUM).W)))

    // for store buffer
    val is_store_num_cmt        = Output(UInt(2.W))

    // for predict and ras
    val predict_fail_cmt        = Output(UInt(10.W)) // opt fanout
    val pred_update_en_cmt      = Output(Bool())
    val pred_branch_target_cmt  = Output(UInt(32.W))
    val pred_pc_cmt             = Output(UInt(32.W))
    val pred_real_jump_cmt      = Output(Bool())
    val pred_br_type_cmt        = Output(UInt(2.W))

    // for csr write
    val csr_addr_cmt            = Output(UInt(14.W))
    val csr_wdata_cmt           = Output(UInt(32.W))
    val csr_we_cmt              = Output(Bool())

    // for dcache
    val rob_index_cmt           = Output(Vec(4, UInt(log2Ceil(n).W)))

    // for exception
    val eentry_global           = Input(UInt(32.W))
    val tlbreentry_global       = Input(UInt(32.W))
    val badv_cmt                = Output(UInt(32.W))
    val exception_cmt           = Output(UInt(8.W))
    val is_eret_cmt             = Output(Bool())
    val interrupt_vec           = Input(UInt(13.W))

    // for tlb
    val tlbwr_en_cmt            = Output(Bool())
    val tlbrd_en_cmt            = Output(Bool())
    val tlbfill_en_cmt          = Output(Bool())
    val tlbsrch_en_cmt          = Output(Bool())
    val invtlb_en_cmt           = Output(Bool())
    val invtlb_op_cmt           = Output(UInt(5.W))
    val invtlb_vaddr_cmt        = Output(UInt(32.W))
    val invtlb_asid_cmt         = Output(UInt(10.W))
    val llbit_set_cmt           = Output(Bool())
    val llbit_clear_cmt         = Output(Bool())

    // for idle
    val idle_en_cmt             = Output(Bool())

    // for priv
    val priv_vec_ex             = Input(UInt(10.W))
    val csr_addr_ex             = Input(UInt(14.W))
    val tlbentry_ex             = Input(new tlb_t)
    val tlbentry_cmt            = Output(new tlb_t)
    val invtlb_op_ex            = Input(UInt(5.W))
    val invtlb_vaddr_ex         = Input(UInt(32.W))
    val invtlb_asid_ex          = Input(UInt(10.W))

    // for ls priv
    val priv_vec_ls             = Input(UInt(3.W))
    val llbit_global            = Input(Bool())

    // diff
    val is_ucread_cmt           = Output(Vec(2, Bool()))
    val rd_cmt                  = Output(Vec(2, UInt(5.W)))
    val rf_wdata_cmt            = Output(Vec(2, UInt(32.W)))
    val branch_target_cmt       = Output(UInt(32.W))
    val pc_cmt                  = Output(Vec(2, UInt(32.W)))
    val csr_diff_addr_cmt       = Output(Vec(2, UInt(14.W)))
    val csr_diff_wdata_cmt      = Output(Vec(2, UInt(32.W)))
    val csr_diff_we_cmt         = Output(Vec(2, Bool()))
    val inst_cmt                = Output(Vec(2, UInt(32.W)))

    // stat
    val predict_fail_stat       = Output(Vec(2, Bool()))
    val br_type_stat            = Output(Vec(2, UInt(2.W)))
    val is_br_stat              = Output(Vec(2, Bool()))
}

class ROB(n: Int) extends Module{
    val io                   = IO(new ROB_IO(n))
    val FRONT_INST_NUM_LOG2  = 1
    val group_num            = n / 2

    // Define the ROB (Reorder Buffer) items
    val rob = RegInit(VecInit.fill(2)(VecInit.fill(group_num)(0.U.asTypeOf(new rob_t))))

    // Define privilege buffers
    val priv_buffer = RegInit(0.U.asTypeOf(new priv_t(n)))
    val priv_ls_buffer = RegInit(0.U.asTypeOf(new priv_ls_t))

    // Define interrupt buffer
    val interrupt_buffer = RegInit(0.U(13.W))

    // Define head and head_next registers
    val head = RegInit(0.U(log2Ceil(n).W))
    val head_next = RegInit(1.U(log2Ceil(n).W))
    val head_group = VecInit(head, head_next)

    // Define tail register
    val tail = RegInit(0.U(log2Ceil(group_num).W))

    // Define element numbers with initial values
    val elem_num    = RegInit(VecInit.fill(10)(VecInit.fill(2)(0.U((log2Ceil(group_num)+1).W))))

    // Define head_select_index and head_index
    val head_select_index = VecInit.tabulate(2) { i =>
        head_group(i)(FRONT_INST_NUM_LOG2 - 1, 0)
    }

    val head_index = VecInit.tabulate(2) { i =>
        head_group(i)(log2Ceil(n) - 1, FRONT_INST_NUM_LOG2)
    }

    // Define empty and full status
    val empty = VecInit(elem_num(0).map(_ === 0.U))
    val full = VecInit.tabulate(10) { i =>
        VecInit(elem_num(i).map(_ === group_num.U)).asUInt.orR
    }


    // for rn stage
        // Define allow_next_cmt based on various conditions
    val allow_next_cmt = VecInit.tabulate(2) { i =>
        (
            !io.pred_update_en_dp(i) ||
            (io.priv_vec_dp(i)(0) && io.priv_vec_dp(i)(12, 1).orR) ||
            io.exception_dp(i)(7)
        )
    }

    when(!full(0)){
        for(i <- 0 until 2){
            when(io.inst_valid_dp(0)){
                rob(i)(tail).rd              := io.rd_dp(i)
                rob(i)(tail).rd_valid        := io.rd_valid_dp(i)
                rob(i)(tail).prd             := io.prd_dp(i)
                rob(i)(tail).pprd            := io.pprd_dp(i)
                rob(i)(tail).pc              := io.pc_dp(i) + 4.U
                rob(i)(tail).is_store        := io.is_store_dp(i)
                rob(i)(tail).br_type_pred    := io.br_type_pred_dp(i)
                rob(i)(tail).pred_update_en  := io.pred_update_en_dp(i)
                rob(i)(tail).predict_fail    := false.B
                rob(i)(tail).complete        := false.B
                rob(i)(tail).is_priv_wrt     := io.priv_vec_dp(i)(0) && io.priv_vec_dp(i)(9, 1).orR
                rob(i)(tail).is_priv_ls      := io.priv_vec_dp(i)(12, 10).orR
                rob(i)(tail).exception       := io.exception_dp(i)
                rob(i)(tail).inst            := io.inst_dp(i)
                rob(i)(tail).allow_next_cmt  := allow_next_cmt(i)
            }
        }
    }
    io.rob_index_dp := VecInit.tabulate(2)(i => tail ## i.U(FRONT_INST_NUM_LOG2.W))
    io.full         := full

    // for ex
        // Handle privilege
    val priv_valid = !priv_buffer.valid && io.priv_vec_ex(0) && io.priv_vec_ex(9, 1).orR
    when(io.predict_fail_cmt(0)){
        priv_buffer.valid          := false.B
    }.elsewhen(priv_valid){
        priv_buffer.csr_addr       := io.csr_addr_ex
        priv_buffer.priv_vec       := io.priv_vec_ex
        priv_buffer.tlb_entry      := io.tlbentry_ex
        priv_buffer.inv_op         := io.invtlb_op_ex
        priv_buffer.inv_vaddr      := io.invtlb_vaddr_ex
        priv_buffer.inv_asid       := io.invtlb_asid_ex
        priv_buffer.valid          := true.B
    }
        
        // Handle ls privilege
    val priv_ls_valid = !priv_ls_buffer.valid && io.priv_vec_ls.orR
    when(io.predict_fail_cmt(0)){
        priv_ls_buffer.valid       := false.B
    }.elsewhen(priv_ls_valid){
        priv_ls_buffer.priv_vec    := io.priv_vec_ls
        priv_ls_buffer.valid       := true.B
    }

        // Handle interrupt
    val new_interrupt = Mux(io.cmt_en(0), 0.U(1.W) ## io.interrupt_vec, 
                        Mux(!interrupt_buffer(12), 1.U(1.W) ## io.interrupt_vec, interrupt_buffer))
    interrupt_buffer := new_interrupt

    // wb stage
    for(i <- 0 until 4){
        when(io.inst_valid_wb(i)){
            val column_index = io.rob_index_wb(i)(FRONT_LOG2-1, 0)
            val row_index = io.rob_index_wb(i)(log2Ceil(n)-1, FRONT_LOG2)
            val rob_entry = rob(column_index)(row_index)
            
            rob_entry.complete := true.B
            rob_entry.rf_wdata := io.rf_wdata_wb(i)
            rob_entry.is_ucread := io.is_ucread_wb(i)
            // Determine branch_target
            if(i != 0){
                if(i == 2){
                    rob_entry.branch_target   := Mux(rob_entry.exception(7), rob_entry.pc, io.branch_target_wb(i))
                }else if(i == 3){
                    rob_entry.branch_target   := io.branch_target_wb(i) + 4.U
                }else{
                    rob_entry.branch_target   := io.branch_target_wb(i)
                }
            }
            
            if(i == 1){
                rob_entry.predict_fail      := io.predict_fail_wb(i)
                rob_entry.real_jump         := io.real_jump_wb(i)
            }
            if(i == 3){
                rob_entry.exception         := io.exception_wb(i)
                rob_entry.allow_next_cmt    := !io.exception_wb(i)(7)
            }
        }
    }
    
    // cmt stage
    val interrupt_vec           = interrupt_buffer(11, 0).orR 
    val cmt_en                  = WireDefault(VecInit.fill(2)(false.B))
    val rob_commit_items        = VecInit.tabulate(2)(i => rob(head_select_index(i))(head_index(i)))

    cmt_en(0)                   := rob_commit_items(0).complete && !empty(head_select_index(0))
    cmt_en(1)                   := rob_commit_items(1).complete && !empty(head_select_index(1)) && cmt_en(0) && rob_commit_items(0).allow_next_cmt && !interrupt_vec
    
    io.cmt_en                   := ShiftRegister(cmt_en, 1)
    io.rob_index_cmt            := ShiftRegister(VecInit.fill(4)(head), 1)

    val eentry_global           = ShiftRegister(io.eentry_global, 1);
    val interrupt               = interrupt_vec && cmt_en(0)

    // update predict and ras
    val rob_update_item         = Mux(cmt_en(0), Mux(cmt_en(1), rob_commit_items(1), rob_commit_items(0)), 0.U.asTypeOf(new rob_t))

    val predict_fail_cmt        = rob_update_item.predict_fail || rob_update_item.is_priv_wrt || rob_update_item.is_priv_ls || rob_update_item.exception(7) || interrupt
    val branch_target_cmt       = Mux(rob_update_item.exception(7) || interrupt_vec, eentry_global, 
                                  Mux(rob_update_item.is_priv_wrt && priv_buffer.priv_vec(3) || rob_update_item.pred_update_en && rob_update_item.real_jump, rob_update_item.branch_target, rob_update_item.pc))
    val pred_update_en_cmt      = rob_update_item.pred_update_en
    val pred_branch_target_cmt  = rob_update_item.branch_target
    val pred_br_type_cmt        = rob_update_item.br_type_pred
    val pred_pc_cmt             = rob_update_item.pc - 4.U
    val pred_real_jump_cmt      = rob_update_item.real_jump
    val exception_cmt           = Mux(interrupt, 0x80.U(8.W), rob_update_item.exception)

    io.predict_fail_cmt         := ShiftRegister(VecInit.fill(10)(predict_fail_cmt).asUInt, 1)
    io.branch_target_cmt        := ShiftRegister(branch_target_cmt, 1)
    io.pred_update_en_cmt       := ShiftRegister(pred_update_en_cmt, 1)
    io.pred_branch_target_cmt   := ShiftRegister(pred_branch_target_cmt, 1)
    io.pred_br_type_cmt         := ShiftRegister(pred_br_type_cmt, 1)
    io.pred_pc_cmt              := ShiftRegister(pred_pc_cmt, 1)
    io.pred_real_jump_cmt       := ShiftRegister(pred_real_jump_cmt, 1)
    io.exception_cmt            := ShiftRegister(exception_cmt, 1)


    // update store buffer
        // Compute the store commit bit for each item
    val is_store_cmt_bit = VecInit.tabulate(2) { i =>
        val item = rob_commit_items(i)
        item.is_store && cmt_en(i) && !item.exception(7) && !(item.is_priv_ls && !RegNext(io.llbit_global))
    }
    // Count the number of store commits
    val is_store_num_cmt = PopCount(is_store_cmt_bit)
    io.is_store_num_cmt         := ShiftRegister(is_store_num_cmt, 1)

    // update csr file
    // Collect all CSR related signals
    val csr_addr_cmt            = priv_buffer.csr_addr
    val csr_wdata_cmt           = rob_update_item.branch_target
    val csr_we_cmt              = rob_update_item.is_priv_wrt && priv_buffer.priv_vec(2, 1).orR
    val is_eret_cmt             = rob_update_item.is_priv_wrt && priv_buffer.priv_vec(3)
    val badv_cmt                = rob_update_item.branch_target - 4.U
    val tlbrd_en_cmt            = 0.U
    val tlbwr_en_cmt            = 0.U
    val tlbfill_en_cmt          = 0.U
    val tlbsrch_en_cmt          = 0.U
    val tlbentry_cmt            = priv_buffer.tlb_entry
    val invtlb_en_cmt           = 0.U
    val idle_en_cmt             = rob_update_item.is_priv_wrt && priv_buffer.priv_vec(9)
    val invtlb_op_cmt           = 0.U
    val invtlb_vaddr_cmt        = 0.U
    val invtlb_asid_cmt         = 0.U
    val llbit_set_cmt           = 0.U
    val llbit_clear_cmt         = 0.U

    io.csr_addr_cmt             := ShiftRegister(csr_addr_cmt, 1)
    io.csr_wdata_cmt            := ShiftRegister(csr_wdata_cmt, 1)
    io.csr_we_cmt               := ShiftRegister(csr_we_cmt, 1)
    io.is_eret_cmt              := ShiftRegister(is_eret_cmt, 1)
    io.badv_cmt                 := ShiftRegister(badv_cmt, 1)
    io.tlbrd_en_cmt             := ShiftRegister(tlbrd_en_cmt, 1)
    io.tlbwr_en_cmt             := ShiftRegister(tlbwr_en_cmt, 1)
    io.tlbfill_en_cmt           := ShiftRegister(tlbfill_en_cmt, 1)
    io.tlbsrch_en_cmt           := ShiftRegister(tlbsrch_en_cmt, 1)
    io.tlbentry_cmt             := ShiftRegister(tlbentry_cmt, 1)
    io.invtlb_en_cmt            := ShiftRegister(invtlb_en_cmt, 1)
    io.invtlb_op_cmt            := ShiftRegister(invtlb_op_cmt, 1)
    io.invtlb_vaddr_cmt         := ShiftRegister(invtlb_vaddr_cmt, 1)
    io.invtlb_asid_cmt          := ShiftRegister(invtlb_asid_cmt, 1)
    io.idle_en_cmt              := ShiftRegister(idle_en_cmt, 1)
    io.llbit_set_cmt            := ShiftRegister(llbit_set_cmt, 1)
    io.llbit_clear_cmt          := ShiftRegister(llbit_clear_cmt, 1)

    // update ptrs
    val cmt_num = PopCount(cmt_en)
    val predict_fail = io.predict_fail_cmt(0) || predict_fail_cmt
    when(predict_fail){
        head := 0.U
        head_next := 1.U
    }.otherwise{
        head := Mux(head + cmt_num >= n.U, head + cmt_num - n.U, head + cmt_num)
        head_next := Mux(head_next + cmt_num >= n.U, head_next + cmt_num - n.U, head_next + cmt_num)
    }

    val head_chosen  = VecInit.fill(2)(false.B)
    for(i <- 0 until 2){
        head_chosen(head_select_index(i))   := cmt_en(i)
        for(j <- 0 until 10){
            when(predict_fail){
                elem_num(j)(i) := 0.U
            }.elsewhen(!full(i) && !io.stall){
                elem_num(j)(i) := elem_num(j)(i) + io.inst_valid_dp(0) - head_chosen(i)
            }.otherwise{
                elem_num(j)(i) := elem_num(j)(i) - head_chosen(i)
            }
        }
    }

    when(predict_fail){
        tail := 0.U
    }.elsewhen(!full(1) && !io.stall){
       tail := Mux(tail + io.inst_valid_dp(0) === group_num.U, 0.U, tail + io.inst_valid_dp(0))
    }

    // stat
    val rd_cmt                   = VecInit.tabulate(2)(i => rob_commit_items(i).rd)
    val rd_valid_cmt             = VecInit.tabulate(2)(i => rob_commit_items(i).rd_valid && !rob_commit_items(i).exception(7))
    val prd_cmt                  = VecInit.tabulate(2)(i => rob_commit_items(i).prd)
    val pprd_cmt                 = VecInit.tabulate(2)(i => rob_commit_items(i).pprd)
    val pc_cmt                   = VecInit.tabulate(2)(i => Mux(rob_commit_items(i).exception(7) || interrupt, eentry_global, 
                                                            Mux(rob_commit_items(i).is_priv_wrt && priv_buffer.priv_vec(3) || rob_commit_items(i).pred_update_en && rob_commit_items(i).real_jump, rob_commit_items(i).branch_target, rob_commit_items(i).pc)))
    val rf_wdata_cmt             = VecInit.tabulate(2)(i => rob_commit_items(i).rf_wdata)
    val is_ucread_cmt            = VecInit.tabulate(2)(i => rob_commit_items(i).is_ucread && cmt_en(i))
    val csr_diff_addr_cmt        = VecInit.fill(2)(priv_buffer.csr_addr)
    val csr_diff_wdata_cmt       = VecInit.fill(2)(rob_update_item.branch_target)
    val csr_diff_we_cmt          = VecInit.tabulate(2)(i => Mux(rob_commit_items(i).is_priv_wrt, priv_buffer.priv_vec(2, 1).orR, false.B))
    val inst_cmt                 = VecInit.tabulate(2)(i => rob_commit_items(i).inst)
    val predict_fail_stat        = VecInit.tabulate(2)(i => rob(head_select_index(i))(head_index(i)).predict_fail & cmt_en(i))
    val br_type_stat             = VecInit.tabulate(2)(i => rob(head_select_index(i))(head_index(i)).br_type_pred)
    val is_br_stat               = VecInit.tabulate(2)(i => rob(head_select_index(i))(head_index(i)).pred_update_en & cmt_en(i))

    io.rd_valid_cmt             := ShiftRegister(rd_valid_cmt, 1)
    io.rd_cmt                   := ShiftRegister(rd_cmt, 1)
    io.prd_cmt                  := ShiftRegister(prd_cmt, 1)
    io.pprd_cmt                 := ShiftRegister(pprd_cmt, 1)
    io.pc_cmt                   := ShiftRegister(pc_cmt, 1)
    io.rf_wdata_cmt             := ShiftRegister(rf_wdata_cmt, 1)
    io.is_ucread_cmt            := ShiftRegister(is_ucread_cmt, 1)
    io.csr_diff_addr_cmt        := ShiftRegister(csr_diff_addr_cmt, 1)
    io.csr_diff_wdata_cmt       := ShiftRegister(csr_diff_wdata_cmt, 1)
    io.csr_diff_we_cmt          := ShiftRegister(csr_diff_we_cmt, 1)
    io.predict_fail_stat        := ShiftRegister(predict_fail_stat, 1)
    io.br_type_stat             := ShiftRegister(br_type_stat, 1)
    io.is_br_stat               := ShiftRegister(is_br_stat, 1)
    io.inst_cmt                 := ShiftRegister(inst_cmt, 1)
} 
module core_top(
  input         aclk,
                aresetn,
  input  [7:0]  intrpt,
  output [31:0] araddr,
  output [1:0]  arburst,
  output [3:0]  arid,
  output [7:0]  arlen,
  input         arready,
  output [2:0]  arsize,
  output        arvalid,
  output [31:0] awaddr,
  output [1:0]  awburst,
  output [3:0]  awid,
  output [7:0]  awlen,
  input         awready,
  output [2:0]  awsize,
  output        awvalid,
  input  [3:0]  bid,
  output        bready,
  input  [1:0]  bresp,
  input         bvalid,
  input  [31:0] rdata,
  input  [3:0]  rid,
  input         rlast,
  output        rready,
  input  [1:0]  rresp,
  input         rvalid,
  output [31:0] wdata,
  output        wlast,
  input         wready,
  output [3:0]  wstrb,
  output        wvalid,
                io_commit_en_0,
                io_commit_en_1,
  output [4:0]  io_commit_rd_0,
                io_commit_rd_1,
  output [5:0]  io_commit_prd_0,
                io_commit_prd_1,
  output        io_commit_rd_valid_0,
                io_commit_rd_valid_1,
  output [31:0] io_commit_rf_wdata_0,
                io_commit_rf_wdata_1,
                io_commit_csr_wdata_0,
                io_commit_csr_wdata_1,
  output        io_commit_csr_we_0,
                io_commit_csr_we_1,
  output [13:0] io_commit_csr_waddr_0,
                io_commit_csr_waddr_1,
  output [31:0] io_commit_pc_0,
                io_commit_pc_1,
  output        io_commit_is_ucread_0,
                io_commit_is_ucread_1,
                io_commit_is_br_0,
                io_commit_is_br_1,
  output [1:0]  io_commit_br_type_0,
                io_commit_br_type_1,
  output        io_commit_predict_fail_0,
                io_commit_predict_fail_1,
  output [31:0] io_commit_inst_0,
                io_commit_inst_1,
  output        io_commit_interrupt,
  output [12:0] io_commit_interrupt_type,
  output        io_commit_stall_by_fetch_queue,
                io_commit_stall_by_rename,
                io_commit_stall_by_rob,
                io_commit_stall_by_iq_0,
                io_commit_stall_by_iq_1,
                io_commit_stall_by_iq_2,
                io_commit_stall_by_iq_3,
                io_commit_stall_by_iq_4,
                io_commit_stall_by_sb,
                io_commit_stall_by_icache,
                io_commit_stall_by_div,
                io_commit_icache_miss,
                io_commit_icache_visit,
                io_commit_stall_by_dcache,
                io_commit_dcache_miss,
                io_commit_dcache_visit,
                io_commit_iq_issue_0,
                io_commit_iq_issue_1,
                io_commit_iq_issue_2,
                io_commit_iq_issue_3,
                io_commit_iq_issue_4,
                io_commit_tlbfill_en,
  output [3:0]  io_commit_tlbfill_idx,
  output [31:0] debug0_wb_pc,
  output [3 :0] debug0_wb_rf_we,
  output [4 :0] debug0_wb_rf_wnum,
  output [31:0] debug0_wb_rf_wdata,

  output [1:0]  arlock,
  output [2:0]  arprot,
  output [3:0]  arcache,
  output [1:0]  awlock,
  output [2:0]  awprot,
  output [3:0]  awcache,
  output [3:0]  wid,

  input           break_point,//无需实现功能，仅提供接口即可，输入1’b0
  input           infor_flag,//无需实现功能，仅提供接口即可，输入1’b0
  input  [ 4:0]   reg_num,//无需实现功能，仅提供接口即可，输入5’b0
  output          ws_valid,//无需实现功能，仅提供接口即可
  output [31:0]   rf_rdata//无需实现功能，仅提供接口即可


);

  logic clock, reset;
  assign clock = aclk;
  assign reset = !aresetn;
  logic [31:0] io_araddr;
  assign araddr = io_araddr;
  logic [1:0] io_arburst;
  assign arburst = io_arburst;
  logic [3:0] io_arid;
  assign arid = io_arid;
  logic [7:0] io_arlen;
  assign arlen = io_arlen;
  logic io_arvalid;
  assign arvalid = io_arvalid;
  logic [2:0] io_arsize;
  assign arsize = io_arsize;
  logic [31:0] io_awaddr;
  assign awaddr = io_awaddr;
  logic [1:0] io_awburst;
  assign awburst = io_awburst;
  logic [3:0] io_awid;
  assign awid = io_awid;
  logic [7:0] io_awlen;
  assign awlen = io_awlen;
  logic io_awvalid;
  assign awvalid = io_awvalid;
  logic [2:0] io_awsize;
  assign awsize = io_awsize;
  logic [3:0] io_bid;
  assign io_bid = bid;
  logic io_bready;
  assign bready = io_bready;
  logic [1:0] io_bresp;
  assign io_bresp = bresp;
  logic io_bvalid;
  assign io_bvalid = bvalid;
  logic [31:0] io_rdata;
  assign io_rdata = rdata;
  logic io_rlast;
  assign io_rlast = rlast;
  logic io_rready;
  assign rready = io_rready;
  logic io_rvalid;
  assign io_rvalid = rvalid;
  logic [31:0] io_wdata;
  assign wdata = io_wdata;
  logic [3:0] io_wstrb;
  assign wstrb = io_wstrb;
  logic io_wlast;
  assign wlast = io_wlast;
  logic io_wready;
  assign io_wready = wready;
  logic io_wvalid;
  assign wvalid = io_wvalid;
  logic io_arready;
  assign io_arready = arready;
  logic io_awready;
  assign io_awready = awready;
  logic [3:0] io_rid;
  assign io_rid = rid;
  logic [1:0] io_rresp;
  assign io_rresp = rresp;
  
  assign debug0_wb_pc = io_commit_pc_0;
  assign debug0_wb_rf_we = {io_commit_rd_valid_0, io_commit_rd_valid_0, io_commit_rd_valid_0, io_commit_rd_valid_0};
  assign debug0_wb_rf_wnum = io_commit_rd_0;
  assign debug0_wb_rf_wdata = io_commit_rf_wdata_0;


  wire             _re_reg4_io_stall_T_2;
  wire             _ir_reg4_io_stall_T_6;
  wire             _dr_reg_io_stall_T;
  wire             _fq_io_next_ready_T_1;
  wire             _bypass_io_forward_prj_en_0;
  wire             _bypass_io_forward_prj_en_1;
  wire             _bypass_io_forward_prj_en_2;
  wire             _bypass_io_forward_prk_en_0;
  wire             _bypass_io_forward_prk_en_1;
  wire             _bypass_io_forward_prk_en_2;
  wire [31:0]      _bypass_io_forward_prj_data_0;
  wire [31:0]      _bypass_io_forward_prj_data_1;
  wire [31:0]      _bypass_io_forward_prj_data_2;
  wire [31:0]      _bypass_io_forward_prk_data_0;
  wire [31:0]      _bypass_io_forward_prk_data_1;
  wire [31:0]      _bypass_io_forward_prk_data_2;
  wire             _rob_io_is_store_dp_0;
  wire             _rob_io_is_store_dp_1;
  wire             _rob_io_pred_update_en_dp_0;
  wire             _rob_io_pred_update_en_dp_1;
  wire [12:0]      _rob_io_interrupt_vec;
  wire [13:0]      _rob_io_csr_addr_ex;
  wire [4:0]       _rob_io_invtlb_op_ex;
  wire [9:0]       _rob_io_invtlb_asid_ex;
  wire [4:0]       _rob_io_rob_index_dp_0;
  wire [4:0]       _rob_io_rob_index_dp_1;
  wire             _rob_io_full_2;
  wire             _rob_io_full_3;
  wire             _rob_io_full_5;
  wire             _rob_io_cmt_en_0;
  wire             _rob_io_cmt_en_1;
  wire [5:0]       _rob_io_prd_cmt_0;
  wire [5:0]       _rob_io_prd_cmt_1;
  wire             _rob_io_rd_valid_cmt_0;
  wire             _rob_io_rd_valid_cmt_1;
  wire [5:0]       _rob_io_pprd_cmt_0;
  wire [5:0]       _rob_io_pprd_cmt_1;
  wire [1:0]       _rob_io_is_store_num_cmt;
  wire [9:0]       _rob_io_predict_fail_cmt;
  wire             _rob_io_pred_update_en_cmt;
  wire [31:0]      _rob_io_pred_pc_cmt;
  wire [1:0]       _rob_io_pred_br_type_cmt;
  wire             _rob_io_csr_we_cmt;
  wire [7:0]       _rob_io_exception_cmt;
  wire             _rob_io_tlbrd_en_cmt;
  wire             _rob_io_tlbfill_en_cmt;
  wire             _ew_reg4_io_flush;
  wire             _ew_reg4_io_inst_pack_WB_rd_valid;
  wire [5:0]       _ew_reg4_io_inst_pack_WB_prd;
  wire [4:0]       _ew_reg4_io_inst_pack_WB_rob_index;
  wire             _ew_reg4_io_inst_pack_WB_inst_valid;
  wire [31:0]      _ew_reg4_io_vaddr_WB;
  wire [7:0]       _ew_reg4_io_exception_WB;
  wire [31:0]      _ew_reg4_io_mem_rdata_WB;
  wire             _ew_reg4_io_is_ucread_WB;
  wire             _ls_ex_mem_reg_io_flush;
  wire             _ls_ex_mem_reg_io_is_ucread_EX;
  wire             _ls_ex_mem_reg_io_inst_pack_MEM_rd_valid;
  wire [5:0]       _ls_ex_mem_reg_io_inst_pack_MEM_prd;
  wire [4:0]       _ls_ex_mem_reg_io_inst_pack_MEM_rob_index;
  wire [4:0]       _ls_ex_mem_reg_io_inst_pack_MEM_mem_type;
  wire [2:0]       _ls_ex_mem_reg_io_inst_pack_MEM_priv_vec;
  wire             _ls_ex_mem_reg_io_inst_pack_MEM_inst_valid;
  wire             _ls_ex_mem_reg_io_is_ucread_MEM;
  wire [31:0]      _ls_ex_mem_reg_io_src1_MEM;
  wire             _ls_ex_mem_reg_io_llbit_MEM;
  wire [63:0]      _ls_ex_mem_reg_io_prd_MEM_0;
  wire [63:0]      _ls_ex_mem_reg_io_prd_MEM_1;
  wire [63:0]      _ls_ex_mem_reg_io_prd_MEM_2;
  wire [7:0]       _ls_ex_mem_reg_io_exception_MEM;
  wire [4:0]       _dcache_io_rob_index_CMT;
  wire             _dcache_io_cacop_en;
  wire [1:0]       _dcache_io_cacop_op;
  wire             _dcache_io_flush;
  wire             _dcache_io_cache_miss_MEM_3;
  wire             _dcache_io_cache_miss_MEM_4;
  wire [31:0]      _dcache_io_rdata_MEM;
  wire [1:0]       _sb_io_is_store_num_cmt;
  wire             _sb_io_dcache_miss;
  wire             _sb_io_flush;
  wire             _sb_io_em_stall;
  wire             _sb_io_full;
  wire             _sb_io_st_cmt_valid;
  wire [31:0]      _sb_io_st_addr_cmt;
  wire [31:0]      _sb_io_st_data_cmt;
  wire [1:0]       _sb_io_st_wlen_cmt;
  wire             _sb_io_is_uncache_cmt;
  wire [31:0]      _sb_io_ld_data_mem;
  wire             _sb_io_ld_hit_0;
  wire             _sb_io_ld_hit_1;
  wire             _sb_io_ld_hit_2;
  wire             _sb_io_ld_hit_3;
  wire             _mmu_io_tlbwr_en;
  wire [3:0]       _mmu_io_tlbfill_idx;
  wire             _mmu_io_tlbfill_en;
  wire             _mmu_io_invtlb_en;
  wire [4:0]       _mmu_io_invtlb_op;
  wire [9:0]       _mmu_io_invtlb_asid;
  wire [31:0]      _mmu_io_invtlb_vaddr;
  wire             _mmu_io_i_valid;
  wire             _mmu_io_d_rvalid;
  wire             _mmu_io_d_wvalid;
  wire [31:0]      _mmu_io_d_vaddr;
  wire [3:0]       _mmu_io_tlbsrch_idx;
  wire             _mmu_io_tlbsrch_hit;
  wire [18:0]      _mmu_io_tlbrd_entry_vppn;
  wire [5:0]       _mmu_io_tlbrd_entry_ps;
  wire             _mmu_io_tlbrd_entry_g;
  wire [9:0]       _mmu_io_tlbrd_entry_asid;
  wire             _mmu_io_tlbrd_entry_e;
  wire [19:0]      _mmu_io_tlbrd_entry_ppn0;
  wire [1:0]       _mmu_io_tlbrd_entry_plv0;
  wire [1:0]       _mmu_io_tlbrd_entry_mat0;
  wire             _mmu_io_tlbrd_entry_d0;
  wire             _mmu_io_tlbrd_entry_v0;
  wire [19:0]      _mmu_io_tlbrd_entry_ppn1;
  wire [1:0]       _mmu_io_tlbrd_entry_plv1;
  wire [1:0]       _mmu_io_tlbrd_entry_mat1;
  wire             _mmu_io_tlbrd_entry_d1;
  wire             _mmu_io_tlbrd_entry_v1;
  wire [31:0]      _mmu_io_i_paddr;
  wire [7:0]       _mmu_io_i_exception;
  wire [31:0]      _mmu_io_d_paddr;
  wire             _mmu_io_d_uncache;
  wire [7:0]       _mmu_io_d_exception;
  wire [7:0]       _exception_ls_io_exception_ls;
  wire             _ew_reg3_io_flush;
  wire [4:0]       _ew_reg3_io_inst_pack_WB_rob_index;
  wire             _ew_reg3_io_inst_pack_WB_inst_valid;
  wire [31:0]      _ew_reg3_io_md_out_WB;
  wire [31:0]      _ew_reg3_io_csr_wdata_WB;
  wire             _md_ex2_ex3_reg_io_flush;
  wire             _md_ex2_ex3_reg_io_inst_pack_EX2_rd_valid;
  wire [5:0]       _md_ex2_ex3_reg_io_inst_pack_EX2_prd;
  wire [4:0]       _md_ex2_ex3_reg_io_inst_pack_EX2_rob_index;
  wire [9:0]       _md_ex2_ex3_reg_io_inst_pack_EX2_priv_vec;
  wire [3:0]       _md_ex2_ex3_reg_io_inst_pack_EX2_alu_op;
  wire             _md_ex2_ex3_reg_io_inst_pack_EX2_inst_valid;
  wire [31:0]      _md_ex2_ex3_reg_io_csr_wdata_EX2;
  wire [31:0]      _md_ex2_ex3_reg_io_csr_rdata_EX2;
  wire             _md_ex1_ex2_reg_io_flush;
  wire             _md_ex1_ex2_reg_io_inst_pack_EX2_rd_valid;
  wire [5:0]       _md_ex1_ex2_reg_io_inst_pack_EX2_prd;
  wire [4:0]       _md_ex1_ex2_reg_io_inst_pack_EX2_rob_index;
  wire [9:0]       _md_ex1_ex2_reg_io_inst_pack_EX2_priv_vec;
  wire [3:0]       _md_ex1_ex2_reg_io_inst_pack_EX2_alu_op;
  wire             _md_ex1_ex2_reg_io_inst_pack_EX2_inst_valid;
  wire [31:0]      _md_ex1_ex2_reg_io_csr_wdata_EX2;
  wire [31:0]      _md_ex1_ex2_reg_io_csr_rdata_EX2;
  wire [4:0]       _mdu_io_md_op;
  wire [31:0]      _mdu_io_mul_out;
  wire [31:0]      _mdu_io_div_out;
  wire             _mdu_io_busy_16;
  wire             _mdu_io_busy_17;
  wire             _mdu_io_busy_18;
  wire             _mdu_io_busy_19;
  wire             _mdu_io_busy_20;
  wire             _ew_reg2_io_flush;
  wire             _ew_reg2_io_inst_pack_WB_rd_valid;
  wire [5:0]       _ew_reg2_io_inst_pack_WB_prd;
  wire [4:0]       _ew_reg2_io_inst_pack_WB_rob_index;
  wire             _ew_reg2_io_inst_pack_WB_inst_valid;
  wire [31:0]      _ew_reg2_io_alu_out_WB;
  wire             _ew_reg2_io_predict_fail_WB;
  wire [31:0]      _ew_reg2_io_branch_target_WB;
  wire             _ew_reg2_io_real_jump_WB;
  wire             _br_io_real_jump;
  wire             _br_io_predict_fail;
  wire [31:0]      _br_io_branch_target;
  wire [31:0]      _alu2_io_src2;
  wire [31:0]      _alu2_io_alu_out;
  wire             _ew_reg1_io_flush;
  wire             _ew_reg1_io_is_ucread_EX;
  wire             _ew_reg1_io_inst_pack_WB_rd_valid;
  wire [5:0]       _ew_reg1_io_inst_pack_WB_prd;
  wire [4:0]       _ew_reg1_io_inst_pack_WB_rob_index;
  wire             _ew_reg1_io_inst_pack_WB_inst_valid;
  wire [31:0]      _ew_reg1_io_alu_out_WB;
  wire             _ew_reg1_io_is_ucread_WB;
  wire [31:0]      _alu1_io_src2;
  wire [31:0]      _alu1_io_alu_out;
  wire             _re_reg4_io_flush;
  wire             _re_reg4_io_inst_pack_EX_rd_valid;
  wire [5:0]       _re_reg4_io_inst_pack_EX_prd;
  wire [31:0]      _re_reg4_io_inst_pack_EX_imm;
  wire [4:0]       _re_reg4_io_inst_pack_EX_rob_index;
  wire [4:0]       _re_reg4_io_inst_pack_EX_mem_type;
  wire [2:0]       _re_reg4_io_inst_pack_EX_priv_vec;
  wire             _re_reg4_io_inst_pack_EX_inst_valid;
  wire [31:0]      _re_reg4_io_src1_EX;
  wire [31:0]      _re_reg4_io_src2_EX;
  wire             _re_reg3_io_flush;
  wire             _re_reg3_io_stall;
  wire             _re_reg3_io_inst_pack_EX_rd_valid;
  wire [5:0]       _re_reg3_io_inst_pack_EX_prd;
  wire [31:0]      _re_reg3_io_inst_pack_EX_imm;
  wire [4:0]       _re_reg3_io_inst_pack_EX_rob_index;
  wire [9:0]       _re_reg3_io_inst_pack_EX_priv_vec;
  wire [3:0]       _re_reg3_io_inst_pack_EX_alu_op;
  wire             _re_reg3_io_inst_pack_EX_inst_valid;
  wire [31:0]      _re_reg3_io_src1_EX;
  wire [31:0]      _re_reg3_io_src2_EX;
  wire [31:0]      _re_reg3_io_csr_rdata_EX;
  wire             _re_reg2_io_flush;
  wire [5:0]       _re_reg2_io_inst_pack_EX_prj;
  wire [5:0]       _re_reg2_io_inst_pack_EX_prk;
  wire             _re_reg2_io_inst_pack_EX_rd_valid;
  wire [5:0]       _re_reg2_io_inst_pack_EX_prd;
  wire [31:0]      _re_reg2_io_inst_pack_EX_imm;
  wire [4:0]       _re_reg2_io_inst_pack_EX_rob_index;
  wire [3:0]       _re_reg2_io_inst_pack_EX_alu_op;
  wire             _re_reg2_io_inst_pack_EX_alu_rs1_sel;
  wire [1:0]       _re_reg2_io_inst_pack_EX_alu_rs2_sel;
  wire [31:0]      _re_reg2_io_inst_pack_EX_pc;
  wire [3:0]       _re_reg2_io_inst_pack_EX_br_type;
  wire             _re_reg2_io_inst_pack_EX_predict_jump;
  wire [31:0]      _re_reg2_io_inst_pack_EX_pred_npc;
  wire             _re_reg2_io_inst_pack_EX_inst_valid;
  wire [31:0]      _re_reg2_io_src1_EX;
  wire [31:0]      _re_reg2_io_src2_EX;
  wire             _re_reg1_io_flush;
  wire [5:0]       _re_reg1_io_inst_pack_EX_prj;
  wire [5:0]       _re_reg1_io_inst_pack_EX_prk;
  wire             _re_reg1_io_inst_pack_EX_rd_valid;
  wire [5:0]       _re_reg1_io_inst_pack_EX_prd;
  wire [31:0]      _re_reg1_io_inst_pack_EX_imm;
  wire [4:0]       _re_reg1_io_inst_pack_EX_rob_index;
  wire [3:0]       _re_reg1_io_inst_pack_EX_alu_op;
  wire             _re_reg1_io_inst_pack_EX_alu_rs1_sel;
  wire [1:0]       _re_reg1_io_inst_pack_EX_alu_rs2_sel;
  wire [31:0]      _re_reg1_io_inst_pack_EX_pc;
  wire             _re_reg1_io_inst_pack_EX_inst_valid;
  wire [31:0]      _re_reg1_io_src1_EX;
  wire [31:0]      _re_reg1_io_src2_EX;
  wire [63:0]      _stable_cnt_io_value;
  wire [13:0]      _csr_rf_io_raddr;
  wire [13:0]      _csr_rf_io_waddr;
  wire             _csr_rf_io_we;
  wire [31:0]      _csr_rf_io_wdata;
  wire [7:0]       _csr_rf_io_exception;
  wire [31:0]      _csr_rf_io_badv_exp;
  wire             _csr_rf_io_is_eret;
  wire [31:0]      _csr_rf_io_pc_exp;
  wire [18:0]      _csr_rf_io_tlbentry_in_vppn;
  wire [5:0]       _csr_rf_io_tlbentry_in_ps;
  wire             _csr_rf_io_tlbentry_in_g;
  wire [9:0]       _csr_rf_io_tlbentry_in_asid;
  wire             _csr_rf_io_tlbentry_in_e;
  wire [19:0]      _csr_rf_io_tlbentry_in_ppn0;
  wire [1:0]       _csr_rf_io_tlbentry_in_plv0;
  wire [1:0]       _csr_rf_io_tlbentry_in_mat0;
  wire             _csr_rf_io_tlbentry_in_d0;
  wire             _csr_rf_io_tlbentry_in_v0;
  wire [19:0]      _csr_rf_io_tlbentry_in_ppn1;
  wire [1:0]       _csr_rf_io_tlbentry_in_plv1;
  wire [1:0]       _csr_rf_io_tlbentry_in_mat1;
  wire             _csr_rf_io_tlbentry_in_d1;
  wire             _csr_rf_io_tlbentry_in_v1;
  wire             _csr_rf_io_tlbrd_en;
  wire             _csr_rf_io_tlbsrch_en;
  wire             _csr_rf_io_llbit_set;
  wire             _csr_rf_io_llbit_clear;
  wire [31:0]      _csr_rf_io_rdata;
  wire [31:0]      _csr_rf_io_eentry_global;
  wire [31:0]      _csr_rf_io_tlbreentry_global;
  wire [11:0]      _csr_rf_io_interrupt_vec;
  wire [9:0]       _csr_rf_io_asid_global;
  wire [1:0]       _csr_rf_io_plv_global;
  wire [18:0]      _csr_rf_io_tlbehi_global;
  wire [3:0]       _csr_rf_io_tlbidx_global;
  wire [5:0]       _csr_rf_io_crmd_trans;
  wire [31:0]      _csr_rf_io_dmw0_global;
  wire [31:0]      _csr_rf_io_dmw1_global;
  wire [18:0]      _csr_rf_io_tlbentry_global_vppn;
  wire [5:0]       _csr_rf_io_tlbentry_global_ps;
  wire             _csr_rf_io_tlbentry_global_g;
  wire [9:0]       _csr_rf_io_tlbentry_global_asid;
  wire             _csr_rf_io_tlbentry_global_e;
  wire [19:0]      _csr_rf_io_tlbentry_global_ppn0;
  wire [1:0]       _csr_rf_io_tlbentry_global_plv0;
  wire [1:0]       _csr_rf_io_tlbentry_global_mat0;
  wire             _csr_rf_io_tlbentry_global_d0;
  wire             _csr_rf_io_tlbentry_global_v0;
  wire [19:0]      _csr_rf_io_tlbentry_global_ppn1;
  wire [1:0]       _csr_rf_io_tlbentry_global_plv1;
  wire [1:0]       _csr_rf_io_tlbentry_global_mat1;
  wire             _csr_rf_io_tlbentry_global_d1;
  wire             _csr_rf_io_tlbentry_global_v1;
  wire             _csr_rf_io_llbit_global;
  wire [5:0]       _rf_io_prd_0;
  wire [5:0]       _rf_io_prd_1;
  wire [5:0]       _rf_io_prd_2;
  wire [5:0]       _rf_io_prd_3;
  wire [31:0]      _rf_io_wdata_0;
  wire [31:0]      _rf_io_wdata_1;
  wire [31:0]      _rf_io_wdata_2;
  wire [31:0]      _rf_io_wdata_3;
  wire             _rf_io_rf_we_0;
  wire             _rf_io_rf_we_1;
  wire             _rf_io_rf_we_2;
  wire             _rf_io_rf_we_3;
  wire [31:0]      _rf_io_prj_data_0;
  wire [31:0]      _rf_io_prj_data_1;
  wire [31:0]      _rf_io_prj_data_2;
  wire [31:0]      _rf_io_prk_data_0;
  wire [31:0]      _rf_io_prk_data_1;
  wire [31:0]      _rf_io_prk_data_2;
  wire             _ir_reg4_io_flush;
  wire [31:0]      _ir_reg4_io_src1_RF;
  wire [31:0]      _ir_reg4_io_src2_RF;
  wire             _ir_reg4_io_forward_prj_en;
  wire             _ir_reg4_io_forward_prk_en;
  wire [31:0]      _ir_reg4_io_forward_prj_data;
  wire [31:0]      _ir_reg4_io_forward_prk_data;
  wire [5:0]       _ir_reg4_io_inst_pack_EX_prj;
  wire [5:0]       _ir_reg4_io_inst_pack_EX_prk;
  wire             _ir_reg4_io_inst_pack_EX_rd_valid;
  wire [5:0]       _ir_reg4_io_inst_pack_EX_prd;
  wire [31:0]      _ir_reg4_io_inst_pack_EX_imm;
  wire [4:0]       _ir_reg4_io_inst_pack_EX_rob_index;
  wire [4:0]       _ir_reg4_io_inst_pack_EX_mem_type;
  wire [2:0]       _ir_reg4_io_inst_pack_EX_priv_vec;
  wire             _ir_reg4_io_inst_pack_EX_inst_valid;
  wire [31:0]      _ir_reg4_io_src1_EX;
  wire [31:0]      _ir_reg4_io_src2_EX;
  wire [31:0]      _ir_reg4_io_csr_rdata_EX;
  wire             _sel4_io_stall;
  wire [5:0]       _sel4_io_inst_issue_inst_prj;
  wire [5:0]       _sel4_io_inst_issue_inst_prk;
  wire             _sel4_io_inst_issue_inst_rd_valid;
  wire [5:0]       _sel4_io_inst_issue_inst_prd;
  wire [31:0]      _sel4_io_inst_issue_inst_imm;
  wire [4:0]       _sel4_io_inst_issue_inst_rob_index;
  wire [4:0]       _sel4_io_inst_issue_inst_mem_type;
  wire [2:0]       _sel4_io_inst_issue_inst_priv_vec;
  wire             _sel4_io_inst_issue_valid;
  wire [4:0]       _iq4_io_insts_dispatch_0_rob_index;
  wire [2:0]       _iq4_io_insts_dispatch_0_priv_vec;
  wire [4:0]       _iq4_io_insts_dispatch_1_rob_index;
  wire [2:0]       _iq4_io_insts_dispatch_1_priv_vec;
  wire [5:0]       _iq4_io_wake_preg_3;
  wire [1:0]       _iq4_io_is_store_cmt_num;
  wire [4:0]       _iq4_io_rob_index_cmt;
  wire             _iq4_io_issue_ack;
  wire             _iq4_io_flush;
  wire [5:0]       _iq4_io_insts_issue_inst_prj;
  wire [5:0]       _iq4_io_insts_issue_inst_prk;
  wire             _iq4_io_insts_issue_inst_rd_valid;
  wire [5:0]       _iq4_io_insts_issue_inst_prd;
  wire [31:0]      _iq4_io_insts_issue_inst_imm;
  wire [4:0]       _iq4_io_insts_issue_inst_rob_index;
  wire [4:0]       _iq4_io_insts_issue_inst_mem_type;
  wire [2:0]       _iq4_io_insts_issue_inst_priv_vec;
  wire             _iq4_io_issue_req;
  wire             _iq4_io_full;
  wire             _ir_reg3_io_flush;
  wire             _ir_reg3_io_stall;
  wire [5:0]       _ir_reg3_io_inst_pack_RF_prj;
  wire [5:0]       _ir_reg3_io_inst_pack_RF_prk;
  wire             _ir_reg3_io_inst_pack_RF_rd_valid;
  wire [5:0]       _ir_reg3_io_inst_pack_RF_prd;
  wire [31:0]      _ir_reg3_io_inst_pack_RF_imm;
  wire [4:0]       _ir_reg3_io_inst_pack_RF_rob_index;
  wire [9:0]       _ir_reg3_io_inst_pack_RF_priv_vec;
  wire [3:0]       _ir_reg3_io_inst_pack_RF_alu_op;
  wire             _ir_reg3_io_inst_pack_RF_inst_valid;
  wire             _sel3_io_stall;
  wire [5:0]       _sel3_io_inst_issue_inst_prj;
  wire [5:0]       _sel3_io_inst_issue_inst_prk;
  wire             _sel3_io_inst_issue_inst_rd_valid;
  wire [5:0]       _sel3_io_inst_issue_inst_prd;
  wire [31:0]      _sel3_io_inst_issue_inst_imm;
  wire [4:0]       _sel3_io_inst_issue_inst_rob_index;
  wire [9:0]       _sel3_io_inst_issue_inst_priv_vec;
  wire [3:0]       _sel3_io_inst_issue_inst_alu_op;
  wire             _sel3_io_inst_issue_valid;
  wire [4:0]       _iq3_io_insts_dispatch_0_rob_index;
  wire [9:0]       _iq3_io_insts_dispatch_0_priv_vec;
  wire [4:0]       _iq3_io_insts_dispatch_1_rob_index;
  wire [9:0]       _iq3_io_insts_dispatch_1_priv_vec;
  wire [5:0]       _iq3_io_ld_mem_prd;
  wire             _iq3_io_issue_ack;
  wire             _iq3_io_flush;
  wire             _iq3_io_dcache_miss;
  wire [5:0]       _iq3_io_insts_issue_inst_prj;
  wire [5:0]       _iq3_io_insts_issue_inst_prk;
  wire             _iq3_io_insts_issue_inst_rd_valid;
  wire [5:0]       _iq3_io_insts_issue_inst_prd;
  wire [31:0]      _iq3_io_insts_issue_inst_imm;
  wire [4:0]       _iq3_io_insts_issue_inst_rob_index;
  wire [9:0]       _iq3_io_insts_issue_inst_priv_vec;
  wire [3:0]       _iq3_io_insts_issue_inst_alu_op;
  wire             _iq3_io_issue_req;
  wire             _iq3_io_full;
  wire             _ir_reg2_io_flush;
  wire [5:0]       _ir_reg2_io_inst_pack_RF_prj;
  wire [5:0]       _ir_reg2_io_inst_pack_RF_prk;
  wire             _ir_reg2_io_inst_pack_RF_rd_valid;
  wire [5:0]       _ir_reg2_io_inst_pack_RF_prd;
  wire [31:0]      _ir_reg2_io_inst_pack_RF_imm;
  wire [4:0]       _ir_reg2_io_inst_pack_RF_rob_index;
  wire [3:0]       _ir_reg2_io_inst_pack_RF_alu_op;
  wire             _ir_reg2_io_inst_pack_RF_alu_rs1_sel;
  wire [1:0]       _ir_reg2_io_inst_pack_RF_alu_rs2_sel;
  wire [31:0]      _ir_reg2_io_inst_pack_RF_pc;
  wire [3:0]       _ir_reg2_io_inst_pack_RF_br_type;
  wire             _ir_reg2_io_inst_pack_RF_predict_jump;
  wire [31:0]      _ir_reg2_io_inst_pack_RF_pred_npc;
  wire             _ir_reg2_io_inst_pack_RF_inst_valid;
  wire             _sel2_io_stall;
  wire [5:0]       _sel2_io_wake_preg;
  wire [5:0]       _sel2_io_inst_issue_inst_prj;
  wire [5:0]       _sel2_io_inst_issue_inst_prk;
  wire             _sel2_io_inst_issue_inst_rd_valid;
  wire [5:0]       _sel2_io_inst_issue_inst_prd;
  wire [31:0]      _sel2_io_inst_issue_inst_imm;
  wire [4:0]       _sel2_io_inst_issue_inst_rob_index;
  wire [3:0]       _sel2_io_inst_issue_inst_alu_op;
  wire             _sel2_io_inst_issue_inst_alu_rs1_sel;
  wire [1:0]       _sel2_io_inst_issue_inst_alu_rs2_sel;
  wire [31:0]      _sel2_io_inst_issue_inst_pc;
  wire [3:0]       _sel2_io_inst_issue_inst_br_type;
  wire             _sel2_io_inst_issue_inst_predict_jump;
  wire [31:0]      _sel2_io_inst_issue_inst_pred_npc;
  wire             _sel2_io_inst_issue_valid;
  wire [4:0]       _iq2_io_insts_dispatch_0_rob_index;
  wire [4:0]       _iq2_io_insts_dispatch_1_rob_index;
  wire [5:0]       _iq2_io_wake_preg_1;
  wire [5:0]       _iq2_io_ld_mem_prd;
  wire             _iq2_io_issue_ack_0;
  wire             _iq2_io_issue_ack_1;
  wire             _iq2_io_issue_ack_2;
  wire             _iq2_io_issue_ack_3;
  wire             _iq2_io_issue_ack_4;
  wire             _iq2_io_issue_ack_5;
  wire             _iq2_io_flush;
  wire             _iq2_io_dcache_miss;
  wire [5:0]       _iq2_io_insts_issue_0_inst_prj;
  wire [5:0]       _iq2_io_insts_issue_0_inst_prk;
  wire             _iq2_io_insts_issue_0_inst_rd_valid;
  wire [5:0]       _iq2_io_insts_issue_0_inst_prd;
  wire [31:0]      _iq2_io_insts_issue_0_inst_imm;
  wire [4:0]       _iq2_io_insts_issue_0_inst_rob_index;
  wire [3:0]       _iq2_io_insts_issue_0_inst_alu_op;
  wire             _iq2_io_insts_issue_0_inst_alu_rs1_sel;
  wire [1:0]       _iq2_io_insts_issue_0_inst_alu_rs2_sel;
  wire [31:0]      _iq2_io_insts_issue_0_inst_pc;
  wire [3:0]       _iq2_io_insts_issue_0_inst_br_type;
  wire             _iq2_io_insts_issue_0_inst_predict_jump;
  wire [31:0]      _iq2_io_insts_issue_0_inst_pred_npc;
  wire [5:0]       _iq2_io_insts_issue_1_inst_prj;
  wire [5:0]       _iq2_io_insts_issue_1_inst_prk;
  wire             _iq2_io_insts_issue_1_inst_rd_valid;
  wire [5:0]       _iq2_io_insts_issue_1_inst_prd;
  wire [31:0]      _iq2_io_insts_issue_1_inst_imm;
  wire [4:0]       _iq2_io_insts_issue_1_inst_rob_index;
  wire [3:0]       _iq2_io_insts_issue_1_inst_alu_op;
  wire             _iq2_io_insts_issue_1_inst_alu_rs1_sel;
  wire [1:0]       _iq2_io_insts_issue_1_inst_alu_rs2_sel;
  wire [31:0]      _iq2_io_insts_issue_1_inst_pc;
  wire [3:0]       _iq2_io_insts_issue_1_inst_br_type;
  wire             _iq2_io_insts_issue_1_inst_predict_jump;
  wire [31:0]      _iq2_io_insts_issue_1_inst_pred_npc;
  wire [5:0]       _iq2_io_insts_issue_2_inst_prj;
  wire [5:0]       _iq2_io_insts_issue_2_inst_prk;
  wire             _iq2_io_insts_issue_2_inst_rd_valid;
  wire [5:0]       _iq2_io_insts_issue_2_inst_prd;
  wire [31:0]      _iq2_io_insts_issue_2_inst_imm;
  wire [4:0]       _iq2_io_insts_issue_2_inst_rob_index;
  wire [3:0]       _iq2_io_insts_issue_2_inst_alu_op;
  wire             _iq2_io_insts_issue_2_inst_alu_rs1_sel;
  wire [1:0]       _iq2_io_insts_issue_2_inst_alu_rs2_sel;
  wire [31:0]      _iq2_io_insts_issue_2_inst_pc;
  wire [3:0]       _iq2_io_insts_issue_2_inst_br_type;
  wire             _iq2_io_insts_issue_2_inst_predict_jump;
  wire [31:0]      _iq2_io_insts_issue_2_inst_pred_npc;
  wire [5:0]       _iq2_io_insts_issue_3_inst_prj;
  wire [5:0]       _iq2_io_insts_issue_3_inst_prk;
  wire             _iq2_io_insts_issue_3_inst_rd_valid;
  wire [5:0]       _iq2_io_insts_issue_3_inst_prd;
  wire [31:0]      _iq2_io_insts_issue_3_inst_imm;
  wire [4:0]       _iq2_io_insts_issue_3_inst_rob_index;
  wire [3:0]       _iq2_io_insts_issue_3_inst_alu_op;
  wire             _iq2_io_insts_issue_3_inst_alu_rs1_sel;
  wire [1:0]       _iq2_io_insts_issue_3_inst_alu_rs2_sel;
  wire [31:0]      _iq2_io_insts_issue_3_inst_pc;
  wire [3:0]       _iq2_io_insts_issue_3_inst_br_type;
  wire             _iq2_io_insts_issue_3_inst_predict_jump;
  wire [31:0]      _iq2_io_insts_issue_3_inst_pred_npc;
  wire [5:0]       _iq2_io_insts_issue_4_inst_prj;
  wire [5:0]       _iq2_io_insts_issue_4_inst_prk;
  wire             _iq2_io_insts_issue_4_inst_rd_valid;
  wire [5:0]       _iq2_io_insts_issue_4_inst_prd;
  wire [31:0]      _iq2_io_insts_issue_4_inst_imm;
  wire [4:0]       _iq2_io_insts_issue_4_inst_rob_index;
  wire [3:0]       _iq2_io_insts_issue_4_inst_alu_op;
  wire             _iq2_io_insts_issue_4_inst_alu_rs1_sel;
  wire [1:0]       _iq2_io_insts_issue_4_inst_alu_rs2_sel;
  wire [31:0]      _iq2_io_insts_issue_4_inst_pc;
  wire [3:0]       _iq2_io_insts_issue_4_inst_br_type;
  wire             _iq2_io_insts_issue_4_inst_predict_jump;
  wire [31:0]      _iq2_io_insts_issue_4_inst_pred_npc;
  wire [5:0]       _iq2_io_insts_issue_5_inst_prj;
  wire [5:0]       _iq2_io_insts_issue_5_inst_prk;
  wire             _iq2_io_insts_issue_5_inst_rd_valid;
  wire [5:0]       _iq2_io_insts_issue_5_inst_prd;
  wire [31:0]      _iq2_io_insts_issue_5_inst_imm;
  wire [4:0]       _iq2_io_insts_issue_5_inst_rob_index;
  wire [3:0]       _iq2_io_insts_issue_5_inst_alu_op;
  wire             _iq2_io_insts_issue_5_inst_alu_rs1_sel;
  wire [1:0]       _iq2_io_insts_issue_5_inst_alu_rs2_sel;
  wire [31:0]      _iq2_io_insts_issue_5_inst_pc;
  wire [3:0]       _iq2_io_insts_issue_5_inst_br_type;
  wire             _iq2_io_insts_issue_5_inst_predict_jump;
  wire [31:0]      _iq2_io_insts_issue_5_inst_pred_npc;
  wire             _iq2_io_issue_req_0;
  wire             _iq2_io_issue_req_1;
  wire             _iq2_io_issue_req_2;
  wire             _iq2_io_issue_req_3;
  wire             _iq2_io_issue_req_4;
  wire             _iq2_io_issue_req_5;
  wire             _iq2_io_full;
  wire             _ir_reg1_io_flush;
  wire [5:0]       _ir_reg1_io_inst_pack_RF_prj;
  wire [5:0]       _ir_reg1_io_inst_pack_RF_prk;
  wire             _ir_reg1_io_inst_pack_RF_rd_valid;
  wire [5:0]       _ir_reg1_io_inst_pack_RF_prd;
  wire [31:0]      _ir_reg1_io_inst_pack_RF_imm;
  wire [4:0]       _ir_reg1_io_inst_pack_RF_rob_index;
  wire [3:0]       _ir_reg1_io_inst_pack_RF_alu_op;
  wire             _ir_reg1_io_inst_pack_RF_alu_rs1_sel;
  wire [1:0]       _ir_reg1_io_inst_pack_RF_alu_rs2_sel;
  wire [31:0]      _ir_reg1_io_inst_pack_RF_pc;
  wire             _ir_reg1_io_inst_pack_RF_inst_valid;
  wire             _sel1_io_stall;
  wire [5:0]       _sel1_io_wake_preg;
  wire [5:0]       _sel1_io_inst_issue_inst_prj;
  wire [5:0]       _sel1_io_inst_issue_inst_prk;
  wire             _sel1_io_inst_issue_inst_rd_valid;
  wire [5:0]       _sel1_io_inst_issue_inst_prd;
  wire [31:0]      _sel1_io_inst_issue_inst_imm;
  wire [4:0]       _sel1_io_inst_issue_inst_rob_index;
  wire [3:0]       _sel1_io_inst_issue_inst_alu_op;
  wire             _sel1_io_inst_issue_inst_alu_rs1_sel;
  wire [1:0]       _sel1_io_inst_issue_inst_alu_rs2_sel;
  wire [31:0]      _sel1_io_inst_issue_inst_pc;
  wire             _sel1_io_inst_issue_valid;
  wire [4:0]       _iq1_io_insts_dispatch_0_rob_index;
  wire [4:0]       _iq1_io_insts_dispatch_1_rob_index;
  wire [5:0]       _iq1_io_wake_preg_0;
  wire [5:0]       _iq1_io_wake_preg_1;
  wire [5:0]       _iq1_io_ld_mem_prd;
  wire             _iq1_io_issue_ack_0;
  wire             _iq1_io_issue_ack_1;
  wire             _iq1_io_issue_ack_2;
  wire             _iq1_io_issue_ack_3;
  wire             _iq1_io_issue_ack_4;
  wire             _iq1_io_issue_ack_5;
  wire             _iq1_io_flush;
  wire             _iq1_io_dcache_miss;
  wire [5:0]       _iq1_io_insts_issue_0_inst_prj;
  wire [5:0]       _iq1_io_insts_issue_0_inst_prk;
  wire             _iq1_io_insts_issue_0_inst_rd_valid;
  wire [5:0]       _iq1_io_insts_issue_0_inst_prd;
  wire [31:0]      _iq1_io_insts_issue_0_inst_imm;
  wire [4:0]       _iq1_io_insts_issue_0_inst_rob_index;
  wire [3:0]       _iq1_io_insts_issue_0_inst_alu_op;
  wire             _iq1_io_insts_issue_0_inst_alu_rs1_sel;
  wire [1:0]       _iq1_io_insts_issue_0_inst_alu_rs2_sel;
  wire [31:0]      _iq1_io_insts_issue_0_inst_pc;
  wire [5:0]       _iq1_io_insts_issue_1_inst_prj;
  wire [5:0]       _iq1_io_insts_issue_1_inst_prk;
  wire             _iq1_io_insts_issue_1_inst_rd_valid;
  wire [5:0]       _iq1_io_insts_issue_1_inst_prd;
  wire [31:0]      _iq1_io_insts_issue_1_inst_imm;
  wire [4:0]       _iq1_io_insts_issue_1_inst_rob_index;
  wire [3:0]       _iq1_io_insts_issue_1_inst_alu_op;
  wire             _iq1_io_insts_issue_1_inst_alu_rs1_sel;
  wire [1:0]       _iq1_io_insts_issue_1_inst_alu_rs2_sel;
  wire [31:0]      _iq1_io_insts_issue_1_inst_pc;
  wire [5:0]       _iq1_io_insts_issue_2_inst_prj;
  wire [5:0]       _iq1_io_insts_issue_2_inst_prk;
  wire             _iq1_io_insts_issue_2_inst_rd_valid;
  wire [5:0]       _iq1_io_insts_issue_2_inst_prd;
  wire [31:0]      _iq1_io_insts_issue_2_inst_imm;
  wire [4:0]       _iq1_io_insts_issue_2_inst_rob_index;
  wire [3:0]       _iq1_io_insts_issue_2_inst_alu_op;
  wire             _iq1_io_insts_issue_2_inst_alu_rs1_sel;
  wire [1:0]       _iq1_io_insts_issue_2_inst_alu_rs2_sel;
  wire [31:0]      _iq1_io_insts_issue_2_inst_pc;
  wire [5:0]       _iq1_io_insts_issue_3_inst_prj;
  wire [5:0]       _iq1_io_insts_issue_3_inst_prk;
  wire             _iq1_io_insts_issue_3_inst_rd_valid;
  wire [5:0]       _iq1_io_insts_issue_3_inst_prd;
  wire [31:0]      _iq1_io_insts_issue_3_inst_imm;
  wire [4:0]       _iq1_io_insts_issue_3_inst_rob_index;
  wire [3:0]       _iq1_io_insts_issue_3_inst_alu_op;
  wire             _iq1_io_insts_issue_3_inst_alu_rs1_sel;
  wire [1:0]       _iq1_io_insts_issue_3_inst_alu_rs2_sel;
  wire [31:0]      _iq1_io_insts_issue_3_inst_pc;
  wire [5:0]       _iq1_io_insts_issue_4_inst_prj;
  wire [5:0]       _iq1_io_insts_issue_4_inst_prk;
  wire             _iq1_io_insts_issue_4_inst_rd_valid;
  wire [5:0]       _iq1_io_insts_issue_4_inst_prd;
  wire [31:0]      _iq1_io_insts_issue_4_inst_imm;
  wire [4:0]       _iq1_io_insts_issue_4_inst_rob_index;
  wire [3:0]       _iq1_io_insts_issue_4_inst_alu_op;
  wire             _iq1_io_insts_issue_4_inst_alu_rs1_sel;
  wire [1:0]       _iq1_io_insts_issue_4_inst_alu_rs2_sel;
  wire [31:0]      _iq1_io_insts_issue_4_inst_pc;
  wire [5:0]       _iq1_io_insts_issue_5_inst_prj;
  wire [5:0]       _iq1_io_insts_issue_5_inst_prk;
  wire             _iq1_io_insts_issue_5_inst_rd_valid;
  wire [5:0]       _iq1_io_insts_issue_5_inst_prd;
  wire [31:0]      _iq1_io_insts_issue_5_inst_imm;
  wire [4:0]       _iq1_io_insts_issue_5_inst_rob_index;
  wire [3:0]       _iq1_io_insts_issue_5_inst_alu_op;
  wire             _iq1_io_insts_issue_5_inst_alu_rs1_sel;
  wire [1:0]       _iq1_io_insts_issue_5_inst_alu_rs2_sel;
  wire [31:0]      _iq1_io_insts_issue_5_inst_pc;
  wire             _iq1_io_issue_req_0;
  wire             _iq1_io_issue_req_1;
  wire             _iq1_io_issue_req_2;
  wire             _iq1_io_issue_req_3;
  wire             _iq1_io_issue_req_4;
  wire             _iq1_io_issue_req_5;
  wire             _iq1_io_full;
  wire [3:0]       _dp_io_elem_num_0;
  wire [3:0]       _dp_io_elem_num_1;
  wire             _dp_io_insts_disp_valid_0_0;
  wire             _dp_io_insts_disp_valid_0_1;
  wire             _dp_io_insts_disp_valid_1_0;
  wire             _dp_io_insts_disp_valid_1_1;
  wire             _dp_io_insts_disp_valid_2_0;
  wire             _dp_io_insts_disp_valid_2_1;
  wire             _dp_io_insts_disp_valid_3_0;
  wire             _dp_io_insts_disp_valid_3_1;
  wire             _rename_io_rename_en_0;
  wire             _rename_io_rename_en_1;
  wire             _rename_io_arch_rat_0;
  wire             _rename_io_arch_rat_1;
  wire             _rename_io_arch_rat_2;
  wire             _rename_io_arch_rat_3;
  wire             _rename_io_arch_rat_4;
  wire             _rename_io_arch_rat_5;
  wire             _rename_io_arch_rat_6;
  wire             _rename_io_arch_rat_7;
  wire             _rename_io_arch_rat_8;
  wire             _rename_io_arch_rat_9;
  wire             _rename_io_arch_rat_10;
  wire             _rename_io_arch_rat_11;
  wire             _rename_io_arch_rat_12;
  wire             _rename_io_arch_rat_13;
  wire             _rename_io_arch_rat_14;
  wire             _rename_io_arch_rat_15;
  wire             _rename_io_arch_rat_16;
  wire             _rename_io_arch_rat_17;
  wire             _rename_io_arch_rat_18;
  wire             _rename_io_arch_rat_19;
  wire             _rename_io_arch_rat_20;
  wire             _rename_io_arch_rat_21;
  wire             _rename_io_arch_rat_22;
  wire             _rename_io_arch_rat_23;
  wire             _rename_io_arch_rat_24;
  wire             _rename_io_arch_rat_25;
  wire             _rename_io_arch_rat_26;
  wire             _rename_io_arch_rat_27;
  wire             _rename_io_arch_rat_28;
  wire             _rename_io_arch_rat_29;
  wire             _rename_io_arch_rat_30;
  wire             _rename_io_arch_rat_31;
  wire             _rename_io_arch_rat_32;
  wire             _rename_io_arch_rat_33;
  wire             _rename_io_arch_rat_34;
  wire             _rename_io_arch_rat_35;
  wire             _rename_io_arch_rat_36;
  wire             _rename_io_arch_rat_37;
  wire             _rename_io_arch_rat_38;
  wire             _rename_io_arch_rat_39;
  wire             _rename_io_arch_rat_40;
  wire             _rename_io_arch_rat_41;
  wire             _rename_io_arch_rat_42;
  wire             _rename_io_arch_rat_43;
  wire             _rename_io_arch_rat_44;
  wire             _rename_io_arch_rat_45;
  wire             _rename_io_arch_rat_46;
  wire             _rename_io_arch_rat_47;
  wire             _rename_io_arch_rat_48;
  wire             _rename_io_arch_rat_49;
  wire             _rename_io_arch_rat_50;
  wire             _rename_io_arch_rat_51;
  wire             _rename_io_arch_rat_52;
  wire             _rename_io_arch_rat_53;
  wire             _rename_io_arch_rat_54;
  wire             _rename_io_arch_rat_55;
  wire             _rename_io_arch_rat_56;
  wire             _rename_io_arch_rat_57;
  wire             _rename_io_arch_rat_58;
  wire             _rename_io_arch_rat_59;
  wire             _rename_io_arch_rat_60;
  wire             _rename_io_arch_rat_61;
  wire             _rename_io_arch_rat_62;
  wire             _rename_io_arch_rat_63;
  wire [5:0]       _rename_io_prd_wake_0;
  wire [5:0]       _rename_io_prd_wake_1;
  wire [5:0]       _rename_io_prd_wake_2;
  wire [5:0]       _rename_io_prd_wake_3;
  wire             _rename_io_wake_valid_0;
  wire             _rename_io_wake_valid_1;
  wire             _rename_io_wake_valid_2;
  wire             _rename_io_wake_valid_3;
  wire [5:0]       _rename_io_prj_0;
  wire [5:0]       _rename_io_prj_1;
  wire [5:0]       _rename_io_prk_0;
  wire [5:0]       _rename_io_prk_1;
  wire [5:0]       _rename_io_prd_0;
  wire [5:0]       _rename_io_prd_1;
  wire [5:0]       _rename_io_pprd_0;
  wire [5:0]       _rename_io_pprd_1;
  wire             _rename_io_prj_ready_0;
  wire             _rename_io_prj_ready_1;
  wire             _rename_io_prk_ready_0;
  wire             _rename_io_prk_ready_1;
  wire             _free_list_io_rename_en_0;
  wire             _free_list_io_rename_en_1;
  wire             _free_list_io_commit_en_0;
  wire             _free_list_io_commit_en_1;
  wire             _free_list_io_commit_pprd_valid_0;
  wire             _free_list_io_commit_pprd_valid_1;
  wire [5:0]       _free_list_io_commit_pprd_0;
  wire [5:0]       _free_list_io_commit_pprd_1;
  wire [5:0]       _free_list_io_head_arch;
  wire             _free_list_io_empty;
  wire             _dr_reg_io_flush;
  wire [3:0]       _dr_reg_io_insts_pack_ID_0_alu_op;
  wire [3:0]       _dr_reg_io_insts_pack_ID_1_alu_op;
  wire [5:0]       _dr_reg_io_alloc_preg_ID_0;
  wire [5:0]       _dr_reg_io_alloc_preg_ID_1;
  wire [31:0]      _dr_reg_io_insts_pack_RN_0_pc;
  wire             _dr_reg_io_insts_pack_RN_0_inst_valid;
  wire             _dr_reg_io_insts_pack_RN_0_predict_jump;
  wire [31:0]      _dr_reg_io_insts_pack_RN_0_pred_npc;
  wire [7:0]       _dr_reg_io_insts_pack_RN_0_exception;
  wire [4:0]       _dr_reg_io_insts_pack_RN_0_rj;
  wire [4:0]       _dr_reg_io_insts_pack_RN_0_rk;
  wire [4:0]       _dr_reg_io_insts_pack_RN_0_rd;
  wire             _dr_reg_io_insts_pack_RN_0_rd_valid;
  wire [31:0]      _dr_reg_io_insts_pack_RN_0_imm;
  wire [3:0]       _dr_reg_io_insts_pack_RN_0_alu_op;
  wire             _dr_reg_io_insts_pack_RN_0_alu_rs1_sel;
  wire [1:0]       _dr_reg_io_insts_pack_RN_0_alu_rs2_sel;
  wire [3:0]       _dr_reg_io_insts_pack_RN_0_br_type;
  wire [4:0]       _dr_reg_io_insts_pack_RN_0_mem_type;
  wire [12:0]      _dr_reg_io_insts_pack_RN_0_priv_vec;
  wire [2:0]       _dr_reg_io_insts_pack_RN_0_fu_id;
  wire [31:0]      _dr_reg_io_insts_pack_RN_1_pc;
  wire             _dr_reg_io_insts_pack_RN_1_inst_valid;
  wire             _dr_reg_io_insts_pack_RN_1_predict_jump;
  wire [31:0]      _dr_reg_io_insts_pack_RN_1_pred_npc;
  wire [7:0]       _dr_reg_io_insts_pack_RN_1_exception;
  wire [4:0]       _dr_reg_io_insts_pack_RN_1_rj;
  wire [4:0]       _dr_reg_io_insts_pack_RN_1_rk;
  wire [4:0]       _dr_reg_io_insts_pack_RN_1_rd;
  wire             _dr_reg_io_insts_pack_RN_1_rd_valid;
  wire [31:0]      _dr_reg_io_insts_pack_RN_1_imm;
  wire [3:0]       _dr_reg_io_insts_pack_RN_1_alu_op;
  wire             _dr_reg_io_insts_pack_RN_1_alu_rs1_sel;
  wire [1:0]       _dr_reg_io_insts_pack_RN_1_alu_rs2_sel;
  wire [3:0]       _dr_reg_io_insts_pack_RN_1_br_type;
  wire [4:0]       _dr_reg_io_insts_pack_RN_1_mem_type;
  wire [12:0]      _dr_reg_io_insts_pack_RN_1_priv_vec;
  wire [2:0]       _dr_reg_io_insts_pack_RN_1_fu_id;
  wire [5:0]       _dr_reg_io_alloc_preg_RN_0;
  wire [5:0]       _dr_reg_io_alloc_preg_RN_1;
  wire [31:0]      _dr_reg_io_inst_RN_0;
  wire [31:0]      _dr_reg_io_inst_RN_1;
  wire [4:0]       _Decode_1_io_rj;
  wire [4:0]       _Decode_1_io_rk;
  wire [4:0]       _Decode_1_io_rd;
  wire             _Decode_1_io_rd_valid;
  wire [31:0]      _Decode_1_io_imm;
  wire [4:0]       _Decode_1_io_alu_op;
  wire             _Decode_1_io_alu_rs1_sel;
  wire [1:0]       _Decode_1_io_alu_rs2_sel;
  wire [3:0]       _Decode_1_io_br_type;
  wire [4:0]       _Decode_1_io_mem_type;
  wire [12:0]      _Decode_1_io_priv_vec;
  wire [2:0]       _Decode_1_io_fu_id;
  wire [7:0]       _Decode_1_io_exception;
  wire [4:0]       _Decode_io_rj;
  wire [4:0]       _Decode_io_rk;
  wire [4:0]       _Decode_io_rd;
  wire             _Decode_io_rd_valid;
  wire [31:0]      _Decode_io_imm;
  wire [4:0]       _Decode_io_alu_op;
  wire             _Decode_io_alu_rs1_sel;
  wire [1:0]       _Decode_io_alu_rs2_sel;
  wire [3:0]       _Decode_io_br_type;
  wire [4:0]       _Decode_io_mem_type;
  wire [12:0]      _Decode_io_priv_vec;
  wire [2:0]       _Decode_io_fu_id;
  wire [7:0]       _Decode_io_exception;
  wire             _fq_io_next_ready;
  wire             _fq_io_flush;
  wire             _fq_io_insts_valid_decode_0;
  wire             _fq_io_insts_valid_decode_1;
  wire [31:0]      _fq_io_insts_pack_id_0_pc;
  wire [31:0]      _fq_io_insts_pack_id_0_inst;
  wire             _fq_io_insts_pack_id_0_predict_jump;
  wire [31:0]      _fq_io_insts_pack_id_0_pred_npc;
  wire [7:0]       _fq_io_insts_pack_id_0_exception;
  wire [31:0]      _fq_io_insts_pack_id_1_pc;
  wire [31:0]      _fq_io_insts_pack_id_1_inst;
  wire             _fq_io_insts_pack_id_1_predict_jump;
  wire [31:0]      _fq_io_insts_pack_id_1_pred_npc;
  wire [7:0]       _fq_io_insts_pack_id_1_exception;
  wire             _fq_io_full;
  wire [31:0]      _pd_io_insts_pack_PD_0_pc;
  wire             _pd_io_insts_pack_PD_0_inst_valid;
  wire             _pd_io_insts_pack_PD_0_predict_jump;
  wire [31:0]      _pd_io_insts_pack_PD_0_pred_npc;
  wire [7:0]       _pd_io_insts_pack_PD_0_exception;
  wire [31:0]      _pd_io_insts_pack_PD_0_inst;
  wire [31:0]      _pd_io_insts_pack_PD_1_pc;
  wire             _pd_io_insts_pack_PD_1_inst_valid;
  wire             _pd_io_insts_pack_PD_1_predict_jump;
  wire [31:0]      _pd_io_insts_pack_PD_1_pred_npc;
  wire [7:0]       _pd_io_insts_pack_PD_1_exception;
  wire [31:0]      _pd_io_insts_pack_PD_1_inst;
  wire             _pd_io_pred_fix;
  wire             _ip_reg_io_flush;
  wire             _ip_reg_io_stall;
  wire [31:0]      _ip_reg_io_npc4_IF_0;
  wire [31:0]      _ip_reg_io_npc4_IF_1;
  wire [31:0]      _ip_reg_io_insts_pack_PD_0_pc;
  wire             _ip_reg_io_insts_pack_PD_0_inst_valid;
  wire             _ip_reg_io_insts_pack_PD_0_predict_jump;
  wire [31:0]      _ip_reg_io_insts_pack_PD_0_pred_npc;
  wire [7:0]       _ip_reg_io_insts_pack_PD_0_exception;
  wire [31:0]      _ip_reg_io_insts_pack_PD_0_inst;
  wire [31:0]      _ip_reg_io_insts_pack_PD_1_pc;
  wire             _ip_reg_io_insts_pack_PD_1_inst_valid;
  wire             _ip_reg_io_insts_pack_PD_1_predict_jump;
  wire [31:0]      _ip_reg_io_insts_pack_PD_1_pred_npc;
  wire [7:0]       _ip_reg_io_insts_pack_PD_1_exception;
  wire [31:0]      _ip_reg_io_insts_pack_PD_1_inst;
  wire [31:0]      _ip_reg_io_npc4_PD_0;
  wire [31:0]      _ip_reg_io_npc4_PD_1;
  wire             _icache_io_rvalid_IF;
  wire             _icache_io_uncache_IF;
  wire             _icache_io_cacop_en;
  wire             _icache_io_exception_RM;
  wire             _icache_io_stall;
  wire             _icache_io_has_cacop_IF;
  wire             _icache_io_cache_miss_RM;
  wire [31:0]      _icache_io_rdata_RM_0;
  wire [31:0]      _icache_io_rdata_RM_1;
  wire             _pi_reg_io_flush;
  wire [31:0]      _pi_reg_io_inst_pack_PF_1_pc;
  wire [31:0]      _pi_reg_io_inst_pack_IF_0_pc;
  wire             _pi_reg_io_inst_pack_IF_0_inst_valid;
  wire             _pi_reg_io_inst_pack_IF_0_predict_jump;
  wire [31:0]      _pi_reg_io_inst_pack_IF_0_pred_npc;
  wire [7:0]       _pi_reg_io_inst_pack_IF_0_exception;
  wire [31:0]      _pi_reg_io_inst_pack_IF_1_pc;
  wire             _pi_reg_io_inst_pack_IF_1_inst_valid;
  wire             _pi_reg_io_inst_pack_IF_1_predict_jump;
  wire [31:0]      _pi_reg_io_inst_pack_IF_1_pred_npc;
  wire [7:0]       _pi_reg_io_inst_pack_IF_1_exception;
  wire [31:0]      _predict_io_pc_cmt;
  wire             _predict_io_real_jump;
  wire [31:0]      _predict_io_branch_target;
  wire             _predict_io_update_en;
  wire [1:0]       _predict_io_br_type;
  wire [2:0]       _predict_io_top_arch;
  wire [31:0]      _predict_io_ras_arch_0;
  wire [31:0]      _predict_io_ras_arch_1;
  wire [31:0]      _predict_io_ras_arch_2;
  wire [31:0]      _predict_io_ras_arch_3;
  wire [31:0]      _predict_io_ras_arch_4;
  wire [31:0]      _predict_io_ras_arch_5;
  wire [31:0]      _predict_io_ras_arch_6;
  wire [31:0]      _predict_io_ras_arch_7;
  wire             _predict_io_predict_fail;
  wire             _predict_io_pd_pred_fix;
  wire             _predict_io_pd_pred_fix_is_bl;
  wire [31:0]      _predict_io_pd_pc_plus_4;
  wire             _predict_io_predict_jump_0;
  wire             _predict_io_predict_jump_1;
  wire [31:0]      _predict_io_pred_npc;
  wire             _pc_io_pc_stall;
  wire             _pc_io_predict_fail;
  wire             _pc_io_pred_jump_0;
  wire             _pc_io_pred_jump_1;
  wire [31:0]      _pc_io_pred_npc;
  wire [31:0]      _pc_io_branch_target;
  wire             _pc_io_flush_by_pd;
  wire [31:0]      _pc_io_flush_pd_target;
  wire             _pc_io_is_idle_cmt;
  wire [31:0]      _pc_io_pc_PF_0;
  wire [31:0]      _pc_io_pc_PF_1;
  wire [31:0]      _pc_io_pc_PF_2;
  wire [31:0]      _pc_io_pc_PF_3;
  wire [31:0]      _pc_io_pc_PF_4;
  wire [31:0]      _pc_io_pc_PF_5;
  wire [31:0]      _pc_io_pc_PF_6;
  wire [31:0]      _pc_io_pc_PF_7;
  wire [31:0]      _pc_io_pc_PF_8;
  wire [31:0]      _pc_io_pc_PF_9;
  wire [31:0]      _pc_io_npc_0;
  wire [31:0]      _pc_io_npc_1;
  wire             _pc_io_inst_valid_PF_0;
  wire             _pc_io_inst_valid_PF_1;
  wire [7:0]       _pc_io_exception_PF;
  wire [31:0]      _arb_io_i_araddr;
  wire             _arb_io_i_rvalid;
  wire [7:0]       _arb_io_i_rlen;
  wire [31:0]      _arb_io_d_araddr;
  wire             _arb_io_d_rvalid;
  wire [2:0]       _arb_io_d_rsize;
  wire [7:0]       _arb_io_d_rlen;
  wire [31:0]      _arb_io_d_awaddr;
  wire             _arb_io_d_wvalid;
  wire [31:0]      _arb_io_d_wdata;
  wire             _arb_io_d_wlast;
  wire [2:0]       _arb_io_d_wsize;
  wire [7:0]       _arb_io_d_wlen;
  wire [3:0]       _arb_io_d_wstrb;
  wire             _arb_io_d_bready;
  wire             _arb_io_i_rready;
  wire [31:0]      _arb_io_i_rdata;
  wire             _arb_io_i_rlast;
  wire             _arb_io_d_rready;
  wire [31:0]      _arb_io_d_rdata;
  wire             _arb_io_d_rlast;
  wire             _arb_io_d_wready;
  wire             _arb_io_d_bvalid;
  wire             stall_by_iq =
    _iq1_io_full | _iq2_io_full | _iq3_io_full | _iq4_io_full;
  wire             _pi_reg_io_stall_T = _fq_io_full | _icache_io_cache_miss_RM;
  reg              pc_io_has_intr_r;
  reg              pc_io_has_csr_change_r;
  wire             _pi_reg_io_stall_T_1 = _pi_reg_io_stall_T | _icache_io_has_cacop_IF;
  reg              icache_io_addr_IF_REG;
  reg  [31:0]      icache_io_addr_IF_REG_1;
  wire [31:0]      _icache_io_addr_IF_T_1 =
    icache_io_addr_IF_REG ? icache_io_addr_IF_REG_1 : _pc_io_pc_PF_9;
  reg              icache_io_paddr_IF_REG;
  reg  [31:0]      icache_io_paddr_IF_REG_1;
  wire [31:0]      _icache_io_paddr_IF_T_1 =
    icache_io_paddr_IF_REG ? icache_io_paddr_IF_REG_1 : _mmu_io_i_paddr;
  reg              icache_io_cacop_en_REG;
  reg              icache_io_cacop_en_REG_1;
  reg  [1:0]       icache_io_cacop_op_REG;
  wire [7:0]       inst_pack_IF_exception =
    _pi_reg_io_inst_pack_IF_0_exception[7]
      ? _pi_reg_io_inst_pack_IF_0_exception
      : _mmu_io_i_exception;
  wire [7:0]       inst_pack_IF_1_exception =
    _pi_reg_io_inst_pack_IF_1_exception[7]
      ? _pi_reg_io_inst_pack_IF_1_exception
      : _mmu_io_i_exception;
  wire [31:0]      fq_io_insts_pack_inst_pack_IF_inst =
    _ip_reg_io_insts_pack_PD_0_exception[7] ? 32'h1C0000 : _pd_io_insts_pack_PD_0_inst;
  wire [7:0]       fq_io_insts_pack_inst_pack_IF_exception =
    _pd_io_insts_pack_PD_0_exception[7]
      ? _pd_io_insts_pack_PD_0_exception
      : _ip_reg_io_insts_pack_PD_0_exception;
  wire [31:0]      fq_io_insts_pack_inst_pack_IF_1_inst =
    _ip_reg_io_insts_pack_PD_1_exception[7] ? 32'h1C0000 : _pd_io_insts_pack_PD_1_inst;
  wire [7:0]       fq_io_insts_pack_inst_pack_IF_1_exception =
    _pd_io_insts_pack_PD_1_exception[7]
      ? _pd_io_insts_pack_PD_1_exception
      : _ip_reg_io_insts_pack_PD_1_exception;
  assign _fq_io_next_ready_T_1 = _rob_io_full_2 | stall_by_iq | _free_list_io_empty;
  reg              free_list_io_predict_fail_r;
  assign _dr_reg_io_stall_T = _rob_io_full_3 | stall_by_iq;
  wire [2:0]       inst_pack_ID_fu_id =
    _fq_io_insts_pack_id_0_exception[7] ? 3'h2 : _Decode_io_fu_id;
  wire [7:0]       inst_pack_ID_exception =
    _fq_io_insts_pack_id_0_exception[7]
      ? _fq_io_insts_pack_id_0_exception
      : _Decode_io_exception;
  wire [2:0]       inst_pack_ID_1_fu_id =
    _fq_io_insts_pack_id_1_exception[7] ? 3'h2 : _Decode_1_io_fu_id;
  wire [7:0]       inst_pack_ID_1_exception =
    _fq_io_insts_pack_id_1_exception[7]
      ? _fq_io_insts_pack_id_1_exception
      : _Decode_1_io_exception;
  reg              rename_io_predict_fail_r;
  wire [5:0]       inst_pack_RN_prd =
    _dr_reg_io_insts_pack_RN_0_rd_valid ? _rename_io_prd_0 : 6'h0;
  wire [5:0]       inst_pack_RN_1_prd =
    _dr_reg_io_insts_pack_RN_1_rd_valid ? _rename_io_prd_1 : 6'h0;
  wire             _br_type_dp_T_3 = _dr_reg_io_insts_pack_RN_0_br_type == 4'h3;
  wire [1:0]       _br_type_dp_T_8 =
    _br_type_dp_T_3 & _dr_reg_io_insts_pack_RN_0_rj == 5'h1
      ? 2'h1
      : {_dr_reg_io_insts_pack_RN_0_br_type == 4'h5, 1'h0};
  wire [1:0]       br_type_dp_0 =
    _br_type_dp_T_3 & _dr_reg_io_insts_pack_RN_0_rd == 5'h1 ? 2'h3 : _br_type_dp_T_8;
  wire             _br_type_dp_T_13 = _dr_reg_io_insts_pack_RN_1_br_type == 4'h3;
  wire [1:0]       _br_type_dp_T_18 =
    _br_type_dp_T_13 & _dr_reg_io_insts_pack_RN_1_rj == 5'h1
      ? 2'h1
      : {_dr_reg_io_insts_pack_RN_1_br_type == 4'h5, 1'h0};
  wire [1:0]       br_type_dp_1 =
    _br_type_dp_T_13 & _dr_reg_io_insts_pack_RN_1_rd == 5'h1 ? 2'h3 : _br_type_dp_T_18;
  wire             _iq4_io_stall_T = stall_by_iq | _rob_io_full_5;
  reg              sel3_io_stall_r;
  reg              sel4_io_stall_r;
  wire [5:0]       iq_inline_wake_preg_2 =
    _mdu_io_busy_17 ? 6'h0 : _md_ex2_ex3_reg_io_inst_pack_EX2_prd;
  wire [5:0]       iq_mutual_wake_preg_2 =
    _mdu_io_busy_18 ? 6'h0 : _md_ex2_ex3_reg_io_inst_pack_EX2_prd;
  wire [5:0]       iq_mutual_wake_preg_3 =
    _dcache_io_cache_miss_MEM_4 ? 6'h0 : _re_reg4_io_inst_pack_EX_prd;
  assign _ir_reg4_io_stall_T_6 =
    _sb_io_full & _re_reg4_io_inst_pack_EX_mem_type[4] | _dcache_io_cache_miss_MEM_3
    | _sb_io_st_cmt_valid & (|(_ir_reg4_io_inst_pack_EX_mem_type[4:3]));
  wire [31:0]      _ir_reg4_io_csr_rdata_RF_T_5 =
    _sel4_io_inst_issue_inst_priv_vec[0]
      ? {{5{_sel4_io_inst_issue_inst_imm[31]}}, _sel4_io_inst_issue_inst_imm[31:5]}
      : _sel4_io_inst_issue_inst_imm;
  wire [31:0]      _re_reg4_io_src1_RF_T =
    _bypass_io_forward_prj_en_2 ? _bypass_io_forward_prj_data_2 : _ir_reg4_io_src1_EX;
  assign _re_reg4_io_stall_T_2 =
    _sb_io_full & _re_reg4_io_inst_pack_EX_mem_type[4] | _dcache_io_cache_miss_MEM_3;
  wire [31:0]      _re_reg4_io_src1_RF_T_1 =
    32'(_re_reg4_io_src1_RF_T + _ir_reg4_io_csr_rdata_EX);
  wire [31:0]      _re_reg4_io_src2_RF_T =
    _bypass_io_forward_prk_en_2 ? _bypass_io_forward_prk_data_2 : _ir_reg4_io_src2_EX;
  wire [31:0]      _alu1_io_src1_T =
    _bypass_io_forward_prj_en_0 ? _bypass_io_forward_prj_data_0 : _re_reg1_io_src1_EX;
  wire [31:0]      _alu1_io_src1_T_2 =
    _re_reg1_io_inst_pack_EX_alu_rs1_sel ? _re_reg1_io_inst_pack_EX_pc : _alu1_io_src1_T;
  wire [31:0]      _alu1_io_src2_T =
    _bypass_io_forward_prk_en_0 ? _bypass_io_forward_prk_data_0 : _re_reg1_io_src2_EX;
  wire [3:0][31:0] _GEN =
    {{_stable_cnt_io_value[31:0]},
     {_stable_cnt_io_value[63:32]},
     {_re_reg1_io_inst_pack_EX_imm},
     {_alu1_io_src2_T}};
  wire [31:0]      _br_io_src1_T =
    _bypass_io_forward_prj_en_1 ? _bypass_io_forward_prj_data_1 : _re_reg2_io_src1_EX;
  wire [31:0]      _alu2_io_src1_T_2 =
    _re_reg2_io_inst_pack_EX_alu_rs1_sel ? _re_reg2_io_inst_pack_EX_pc : _br_io_src1_T;
  wire [31:0]      _br_io_src2_T =
    _bypass_io_forward_prk_en_1 ? _bypass_io_forward_prk_data_1 : _re_reg2_io_src2_EX;
  wire [3:0][31:0] _GEN_0 =
    {{32'h0}, {32'h4}, {_re_reg2_io_inst_pack_EX_imm}, {_br_io_src2_T}};
  wire [31:0]      _csr_wdata_T_5 =
    _re_reg3_io_inst_pack_EX_priv_vec[2]
      ? _re_reg3_io_src1_EX & _re_reg3_io_src2_EX | ~_re_reg3_io_src1_EX
        & _re_reg3_io_csr_rdata_EX
      : _re_reg3_io_src2_EX;
  wire [31:0]      _csr_wdata_T_6 =
    _re_reg3_io_inst_pack_EX_priv_vec[3] ? _re_reg3_io_csr_rdata_EX : _csr_wdata_T_5;
  wire [31:0]      csr_wdata =
    _re_reg3_io_inst_pack_EX_priv_vec[7]
      ? {27'h0, _mmu_io_tlbsrch_hit, _mmu_io_tlbsrch_idx}
      : _csr_wdata_T_6;
  wire [7:0]       _exception_EX_T_9 =
    _exception_ls_io_exception_ls[7]
      ? _exception_ls_io_exception_ls
      : _mmu_io_d_exception;
  wire [7:0]       exception_EX =
    _re_reg4_io_inst_pack_EX_priv_vec[0] & _re_reg4_io_inst_pack_EX_imm[4:3] != 2'h2
    | _re_reg4_io_inst_pack_EX_priv_vec[2] & ~_csr_rf_io_llbit_global
      ? 8'h0
      : _exception_EX_T_9;
  wire [4:0]       _sb_io_mem_type_ex_T =
    _re_reg4_io_stall_T_2 ? 5'h0 : _re_reg4_io_inst_pack_EX_mem_type;
  wire [63:0]      _GEN_1 = {58'h0, _re_reg4_io_inst_pack_EX_prd};
  wire [31:0]      _dcache_io_addr_RF_T =
    _sb_io_st_cmt_valid ? _sb_io_st_addr_cmt : _re_reg4_io_src1_RF_T_1;
  wire [4:0]       _dcache_io_mem_type_RF_T_2 =
    _sb_io_is_uncache_cmt ? 5'h0 : {3'h4, _sb_io_st_wlen_cmt};
  wire [4:0]       _dcache_io_mem_type_RF_T_5 =
    _sb_io_full & _re_reg4_io_inst_pack_EX_mem_type[4]
      ? 5'h0
      : _ir_reg4_io_inst_pack_EX_mem_type;
  wire [4:0]       _dcache_io_mem_type_RF_T_6 =
    _sb_io_st_cmt_valid ? _dcache_io_mem_type_RF_T_2 : _dcache_io_mem_type_RF_T_5;
  wire [31:0]      _dcache_io_wdata_RF_T =
    _sb_io_st_cmt_valid ? _sb_io_st_data_cmt : _re_reg4_io_src2_RF_T;
  wire [7:0]       _mem_rdata_raw_T_2 =
    _sb_io_ld_hit_0 ? _sb_io_ld_data_mem[7:0] : _dcache_io_rdata_MEM[7:0];
  wire [7:0]       _mem_rdata_raw_T_5 =
    _sb_io_ld_hit_1 ? _sb_io_ld_data_mem[15:8] : _dcache_io_rdata_MEM[15:8];
  wire [7:0]       _mem_rdata_raw_T_8 =
    _sb_io_ld_hit_2 ? _sb_io_ld_data_mem[23:16] : _dcache_io_rdata_MEM[23:16];
  wire [7:0]       _mem_rdata_raw_T_11 =
    _sb_io_ld_hit_3 ? _sb_io_ld_data_mem[31:24] : _dcache_io_rdata_MEM[31:24];
  wire [7:0][31:0] _GEN_2 =
    {{32'h0},
     {32'h0},
     {{16'h0, _mem_rdata_raw_T_5, _mem_rdata_raw_T_2}},
     {{24'h0, _mem_rdata_raw_T_2}},
     {32'h0},
     {{_mem_rdata_raw_T_11, _mem_rdata_raw_T_8, _mem_rdata_raw_T_5, _mem_rdata_raw_T_2}},
     {{{16{_mem_rdata_raw_T_5[7]}}, _mem_rdata_raw_T_5, _mem_rdata_raw_T_2}},
     {{{24{_mem_rdata_raw_T_2[7]}}, _mem_rdata_raw_T_2}}};
  wire [31:0]      ls_wb_data =
    _ls_ex_mem_reg_io_inst_pack_MEM_priv_vec[2]
      ? {31'h0, _ls_ex_mem_reg_io_llbit_MEM}
      : _GEN_2[_ls_ex_mem_reg_io_inst_pack_MEM_mem_type[2:0]];
  wire [31:0]      _ew_reg3_io_md_out_EX_T_3 =
    _md_ex2_ex3_reg_io_inst_pack_EX2_alu_op[2] ? _mdu_io_div_out : _mdu_io_mul_out;
  wire [31:0]      _ew_reg3_io_md_out_EX_T_4 =
    _md_ex2_ex3_reg_io_inst_pack_EX2_priv_vec[0]
      ? _md_ex2_ex3_reg_io_csr_rdata_EX2
      : _ew_reg3_io_md_out_EX_T_3;
  always @(posedge clock) begin
    if (reset) begin
      pc_io_has_intr_r <= 1'h0;
      pc_io_has_csr_change_r <= 1'h0;
    end
    else begin
      pc_io_has_intr_r <= |_csr_rf_io_interrupt_vec;
      pc_io_has_csr_change_r <= _rob_io_tlbrd_en_cmt | _rob_io_csr_we_cmt;
    end
    icache_io_addr_IF_REG <= _re_reg4_io_inst_pack_EX_priv_vec[0];
    icache_io_addr_IF_REG_1 <= _re_reg4_io_src1_EX;
    icache_io_paddr_IF_REG <= _re_reg4_io_inst_pack_EX_priv_vec[0];
    icache_io_paddr_IF_REG_1 <= _mmu_io_d_paddr;
    icache_io_cacop_en_REG <= _re_reg4_io_inst_pack_EX_priv_vec[0];
    icache_io_cacop_en_REG_1 <= _re_reg4_io_inst_pack_EX_imm[2:0] == 3'h0;
    icache_io_cacop_op_REG <= _re_reg4_io_inst_pack_EX_imm[4:3];
    free_list_io_predict_fail_r <= _rob_io_predict_fail_cmt[3];
    rename_io_predict_fail_r <= _rob_io_predict_fail_cmt[4];
    sel3_io_stall_r <= _mdu_io_busy_17;
    sel4_io_stall_r <= _ir_reg4_io_stall_T_6;
  end // always @(posedge)
  AXI_Arbiter arb (
    .clock       (clock),
    .reset       (reset),
    .io_i_araddr (_arb_io_i_araddr),
    .io_i_rvalid (_arb_io_i_rvalid),
    .io_i_rready (_arb_io_i_rready),
    .io_i_rdata  (_arb_io_i_rdata),
    .io_i_rlast  (_arb_io_i_rlast),
    .io_i_rlen   (_arb_io_i_rlen),
    .io_d_araddr (_arb_io_d_araddr),
    .io_d_rvalid (_arb_io_d_rvalid),
    .io_d_rready (_arb_io_d_rready),
    .io_d_rdata  (_arb_io_d_rdata),
    .io_d_rlast  (_arb_io_d_rlast),
    .io_d_rsize  (_arb_io_d_rsize),
    .io_d_rlen   (_arb_io_d_rlen),
    .io_d_awaddr (_arb_io_d_awaddr),
    .io_d_wvalid (_arb_io_d_wvalid),
    .io_d_wready (_arb_io_d_wready),
    .io_d_wdata  (_arb_io_d_wdata),
    .io_d_wlast  (_arb_io_d_wlast),
    .io_d_wsize  (_arb_io_d_wsize),
    .io_d_wlen   (_arb_io_d_wlen),
    .io_d_wstrb  (_arb_io_d_wstrb),
    .io_d_bvalid (_arb_io_d_bvalid),
    .io_d_bready (_arb_io_d_bready),
    .io_araddr   (io_araddr),
    .io_arlen    (io_arlen),
    .io_arready  (io_arready),
    .io_arsize   (io_arsize),
    .io_arvalid  (io_arvalid),
    .io_awaddr   (io_awaddr),
    .io_awlen    (io_awlen),
    .io_awready  (io_awready),
    .io_awsize   (io_awsize),
    .io_awvalid  (io_awvalid),
    .io_bready   (io_bready),
    .io_bvalid   (io_bvalid),
    .io_rdata    (io_rdata),
    .io_rlast    (io_rlast),
    .io_rready   (io_rready),
    .io_rvalid   (io_rvalid),
    .io_wdata    (io_wdata),
    .io_wlast    (io_wlast),
    .io_wready   (io_wready),
    .io_wstrb    (io_wstrb),
    .io_wvalid   (io_wvalid)
  );
  assign _pc_io_pc_stall = _pi_reg_io_stall_T | _icache_io_has_cacop_IF;
  assign _pc_io_predict_fail = _rob_io_predict_fail_cmt[0];
  assign _pc_io_pred_jump_0 = _predict_io_predict_jump_0;
  assign _pc_io_pred_jump_1 = _predict_io_predict_jump_1;
  assign _pc_io_pred_npc = _predict_io_pred_npc;
  assign _pc_io_flush_by_pd = _pd_io_pred_fix;
  PC pc (
    .clock              (clock),
    .reset              (reset),
    .io_pc_PF_0         (_pc_io_pc_PF_0),
    .io_pc_PF_1         (_pc_io_pc_PF_1),
    .io_pc_PF_2         (_pc_io_pc_PF_2),
    .io_pc_PF_3         (_pc_io_pc_PF_3),
    .io_pc_PF_4         (_pc_io_pc_PF_4),
    .io_pc_PF_5         (_pc_io_pc_PF_5),
    .io_pc_PF_6         (_pc_io_pc_PF_6),
    .io_pc_PF_7         (_pc_io_pc_PF_7),
    .io_pc_PF_8         (_pc_io_pc_PF_8),
    .io_pc_PF_9         (_pc_io_pc_PF_9),
    .io_pc_stall        (_pc_io_pc_stall),
    .io_predict_fail    (_pc_io_predict_fail),
    .io_npc_0           (_pc_io_npc_0),
    .io_npc_1           (_pc_io_npc_1),
    .io_pred_jump_0     (_pc_io_pred_jump_0),
    .io_pred_jump_1     (_pc_io_pred_jump_1),
    .io_pred_npc        (_pc_io_pred_npc),
    .io_branch_target   (_pc_io_branch_target),
    .io_inst_valid_PF_0 (_pc_io_inst_valid_PF_0),
    .io_inst_valid_PF_1 (_pc_io_inst_valid_PF_1),
    .io_exception_PF    (_pc_io_exception_PF),
    .io_flush_by_pd     (_pc_io_flush_by_pd),
    .io_flush_pd_target (_pc_io_flush_pd_target),
    .io_is_idle_cmt     (_pc_io_is_idle_cmt),
    .io_has_intr        (pc_io_has_intr_r),
    .io_has_csr_change  (pc_io_has_csr_change_r)
  );
  assign _predict_io_pc_cmt = _rob_io_pred_pc_cmt;
  assign _predict_io_update_en = _rob_io_pred_update_en_cmt;
  assign _predict_io_br_type = _rob_io_pred_br_type_cmt;
  assign _predict_io_predict_fail = _rob_io_predict_fail_cmt[0];
  assign _predict_io_pd_pred_fix = _pd_io_pred_fix;
  Predict predict (
    .clock                (clock),
    .reset                (reset),
    .io_npc_0             (_pc_io_npc_0),
    .io_npc_1             (_pc_io_npc_1),
    .io_pc_0              (_pc_io_pc_PF_0),
    .io_pc_1              (_pc_io_pc_PF_1),
    .io_pc_2              (_pc_io_pc_PF_2),
    .io_pc_3              (_pc_io_pc_PF_3),
    .io_pc_4              (_pc_io_pc_PF_4),
    .io_pc_5              (_pc_io_pc_PF_5),
    .io_pc_6              (_pc_io_pc_PF_6),
    .io_predict_jump_0    (_predict_io_predict_jump_0),
    .io_predict_jump_1    (_predict_io_predict_jump_1),
    .io_pred_npc          (_predict_io_pred_npc),
    .io_pc_cmt            (_predict_io_pc_cmt),
    .io_real_jump         (_predict_io_real_jump),
    .io_branch_target     (_predict_io_branch_target),
    .io_update_en         (_predict_io_update_en),
    .io_br_type           (_predict_io_br_type),
    .io_top_arch          (_predict_io_top_arch),
    .io_ras_arch_0        (_predict_io_ras_arch_0),
    .io_ras_arch_1        (_predict_io_ras_arch_1),
    .io_ras_arch_2        (_predict_io_ras_arch_2),
    .io_ras_arch_3        (_predict_io_ras_arch_3),
    .io_ras_arch_4        (_predict_io_ras_arch_4),
    .io_ras_arch_5        (_predict_io_ras_arch_5),
    .io_ras_arch_6        (_predict_io_ras_arch_6),
    .io_ras_arch_7        (_predict_io_ras_arch_7),
    .io_predict_fail      (_predict_io_predict_fail),
    .io_pd_pred_fix       (_predict_io_pd_pred_fix),
    .io_pd_pred_fix_is_bl (_predict_io_pd_pred_fix_is_bl),
    .io_pd_pc_plus_4      (_predict_io_pd_pc_plus_4)
  );
  assign _pi_reg_io_flush = _rob_io_predict_fail_cmt[0] | ~_fq_io_full & _pd_io_pred_fix;
  assign _pi_reg_io_inst_pack_PF_1_pc = 32'(_pc_io_pc_PF_8 + 32'h4);
  PF_IF_Reg pi_reg (
    .clock                          (clock),
    .reset                          (reset),
    .io_flush                       (_pi_reg_io_flush),
    .io_stall                       (_pi_reg_io_stall_T_1),
    .io_inst_pack_PF_0_pc           (_pc_io_pc_PF_8),
    .io_inst_pack_PF_0_inst_valid   (_pc_io_inst_valid_PF_0),
    .io_inst_pack_PF_0_predict_jump (_predict_io_predict_jump_0),
    .io_inst_pack_PF_0_pred_npc     (_predict_io_pred_npc),
    .io_inst_pack_PF_0_exception    (_pc_io_exception_PF),
    .io_inst_pack_PF_1_pc           (_pi_reg_io_inst_pack_PF_1_pc),
    .io_inst_pack_PF_1_inst_valid   (_pc_io_inst_valid_PF_1),
    .io_inst_pack_PF_1_predict_jump (_predict_io_predict_jump_1),
    .io_inst_pack_PF_1_pred_npc     (_predict_io_pred_npc),
    .io_inst_pack_PF_1_exception    (_pc_io_exception_PF),
    .io_inst_pack_IF_0_pc           (_pi_reg_io_inst_pack_IF_0_pc),
    .io_inst_pack_IF_0_inst_valid   (_pi_reg_io_inst_pack_IF_0_inst_valid),
    .io_inst_pack_IF_0_predict_jump (_pi_reg_io_inst_pack_IF_0_predict_jump),
    .io_inst_pack_IF_0_pred_npc     (_pi_reg_io_inst_pack_IF_0_pred_npc),
    .io_inst_pack_IF_0_exception    (_pi_reg_io_inst_pack_IF_0_exception),
    .io_inst_pack_IF_1_pc           (_pi_reg_io_inst_pack_IF_1_pc),
    .io_inst_pack_IF_1_inst_valid   (_pi_reg_io_inst_pack_IF_1_inst_valid),
    .io_inst_pack_IF_1_predict_jump (_pi_reg_io_inst_pack_IF_1_predict_jump),
    .io_inst_pack_IF_1_pred_npc     (_pi_reg_io_inst_pack_IF_1_pred_npc),
    .io_inst_pack_IF_1_exception    (_pi_reg_io_inst_pack_IF_1_exception)
  );
  assign _icache_io_rvalid_IF = ~reset;
  assign _icache_io_cacop_en = icache_io_cacop_en_REG & icache_io_cacop_en_REG_1;
  assign _icache_io_exception_RM = _mmu_io_i_exception[7];
  assign _icache_io_stall = _fq_io_full;
  ICache icache (
    .clock                  (clock),
    .reset                  (reset),
    .io_addr_IF             (_icache_io_addr_IF_T_1),
    .io_paddr_IF            (_icache_io_paddr_IF_T_1),
    .io_rvalid_IF           (_icache_io_rvalid_IF),
    .io_uncache_IF          (_icache_io_uncache_IF),
    .io_cacop_en            (_icache_io_cacop_en),
    .io_cacop_op            (icache_io_cacop_op_REG),
    .io_has_cacop_IF        (_icache_io_has_cacop_IF),
    .io_cache_miss_RM       (_icache_io_cache_miss_RM),
    .io_rdata_RM_0          (_icache_io_rdata_RM_0),
    .io_rdata_RM_1          (_icache_io_rdata_RM_1),
    .io_exception_RM        (_icache_io_exception_RM),
    .io_stall               (_icache_io_stall),
    .io_i_araddr            (_arb_io_i_araddr),
    .io_i_rvalid            (_arb_io_i_rvalid),
    .io_i_rready            (_arb_io_i_rready),
    .io_i_rdata             (_arb_io_i_rdata),
    .io_i_rlast             (_arb_io_i_rlast),
    .io_i_rlen              (_arb_io_i_rlen),
    .io_commit_icache_visit (io_commit_icache_visit),
    .io_commit_icache_miss  (io_commit_icache_miss)
  );
  assign _ip_reg_io_flush =
    _rob_io_predict_fail_cmt[1] | ~_fq_io_full
    & (_pd_io_pred_fix | _icache_io_cache_miss_RM);
  assign _ip_reg_io_stall = _fq_io_full;
  assign _ip_reg_io_npc4_IF_0 = 32'(_pi_reg_io_inst_pack_IF_0_pc + 32'h4);
  assign _ip_reg_io_npc4_IF_1 = 32'(_pi_reg_io_inst_pack_IF_1_pc + 32'h4);
  IF_PD_Reg ip_reg (
    .clock                           (clock),
    .reset                           (reset),
    .io_flush                        (_ip_reg_io_flush),
    .io_stall                        (_ip_reg_io_stall),
    .io_insts_pack_IF_0_pc           (_pi_reg_io_inst_pack_IF_0_pc),
    .io_insts_pack_IF_0_inst_valid   (_pi_reg_io_inst_pack_IF_0_inst_valid),
    .io_insts_pack_IF_0_predict_jump (_pi_reg_io_inst_pack_IF_0_predict_jump),
    .io_insts_pack_IF_0_pred_npc     (_pi_reg_io_inst_pack_IF_0_pred_npc),
    .io_insts_pack_IF_0_exception    (inst_pack_IF_exception),
    .io_insts_pack_IF_0_inst         (_icache_io_rdata_RM_0),
    .io_insts_pack_IF_1_pc           (_pi_reg_io_inst_pack_IF_1_pc),
    .io_insts_pack_IF_1_inst_valid   (_pi_reg_io_inst_pack_IF_1_inst_valid),
    .io_insts_pack_IF_1_predict_jump (_pi_reg_io_inst_pack_IF_1_predict_jump),
    .io_insts_pack_IF_1_pred_npc     (_pi_reg_io_inst_pack_IF_1_pred_npc),
    .io_insts_pack_IF_1_exception    (inst_pack_IF_1_exception),
    .io_insts_pack_IF_1_inst         (_icache_io_rdata_RM_1),
    .io_npc4_IF_0                    (_ip_reg_io_npc4_IF_0),
    .io_npc4_IF_1                    (_ip_reg_io_npc4_IF_1),
    .io_insts_pack_PD_0_pc           (_ip_reg_io_insts_pack_PD_0_pc),
    .io_insts_pack_PD_0_inst_valid   (_ip_reg_io_insts_pack_PD_0_inst_valid),
    .io_insts_pack_PD_0_predict_jump (_ip_reg_io_insts_pack_PD_0_predict_jump),
    .io_insts_pack_PD_0_pred_npc     (_ip_reg_io_insts_pack_PD_0_pred_npc),
    .io_insts_pack_PD_0_exception    (_ip_reg_io_insts_pack_PD_0_exception),
    .io_insts_pack_PD_0_inst         (_ip_reg_io_insts_pack_PD_0_inst),
    .io_insts_pack_PD_1_pc           (_ip_reg_io_insts_pack_PD_1_pc),
    .io_insts_pack_PD_1_inst_valid   (_ip_reg_io_insts_pack_PD_1_inst_valid),
    .io_insts_pack_PD_1_predict_jump (_ip_reg_io_insts_pack_PD_1_predict_jump),
    .io_insts_pack_PD_1_pred_npc     (_ip_reg_io_insts_pack_PD_1_pred_npc),
    .io_insts_pack_PD_1_exception    (_ip_reg_io_insts_pack_PD_1_exception),
    .io_insts_pack_PD_1_inst         (_ip_reg_io_insts_pack_PD_1_inst),
    .io_npc4_PD_0                    (_ip_reg_io_npc4_PD_0),
    .io_npc4_PD_1                    (_ip_reg_io_npc4_PD_1)
  );
  Prev_Decode pd (
    .io_insts_pack_IF_0_pc           (_ip_reg_io_insts_pack_PD_0_pc),
    .io_insts_pack_IF_0_inst_valid   (_ip_reg_io_insts_pack_PD_0_inst_valid),
    .io_insts_pack_IF_0_predict_jump (_ip_reg_io_insts_pack_PD_0_predict_jump),
    .io_insts_pack_IF_0_pred_npc     (_ip_reg_io_insts_pack_PD_0_pred_npc),
    .io_insts_pack_IF_0_exception    (_ip_reg_io_insts_pack_PD_0_exception),
    .io_insts_pack_IF_0_inst         (_ip_reg_io_insts_pack_PD_0_inst),
    .io_insts_pack_IF_1_pc           (_ip_reg_io_insts_pack_PD_1_pc),
    .io_insts_pack_IF_1_inst_valid   (_ip_reg_io_insts_pack_PD_1_inst_valid),
    .io_insts_pack_IF_1_predict_jump (_ip_reg_io_insts_pack_PD_1_predict_jump),
    .io_insts_pack_IF_1_pred_npc     (_ip_reg_io_insts_pack_PD_1_pred_npc),
    .io_insts_pack_IF_1_exception    (_ip_reg_io_insts_pack_PD_1_exception),
    .io_insts_pack_IF_1_inst         (_ip_reg_io_insts_pack_PD_1_inst),
    .io_npc4_IF_0                    (_ip_reg_io_npc4_PD_0),
    .io_npc4_IF_1                    (_ip_reg_io_npc4_PD_1),
    .io_insts_pack_PD_0_pc           (_pd_io_insts_pack_PD_0_pc),
    .io_insts_pack_PD_0_inst_valid   (_pd_io_insts_pack_PD_0_inst_valid),
    .io_insts_pack_PD_0_predict_jump (_pd_io_insts_pack_PD_0_predict_jump),
    .io_insts_pack_PD_0_pred_npc     (_pd_io_insts_pack_PD_0_pred_npc),
    .io_insts_pack_PD_0_exception    (_pd_io_insts_pack_PD_0_exception),
    .io_insts_pack_PD_0_inst         (_pd_io_insts_pack_PD_0_inst),
    .io_insts_pack_PD_1_pc           (_pd_io_insts_pack_PD_1_pc),
    .io_insts_pack_PD_1_inst_valid   (_pd_io_insts_pack_PD_1_inst_valid),
    .io_insts_pack_PD_1_predict_jump (_pd_io_insts_pack_PD_1_predict_jump),
    .io_insts_pack_PD_1_pred_npc     (_pd_io_insts_pack_PD_1_pred_npc),
    .io_insts_pack_PD_1_exception    (_pd_io_insts_pack_PD_1_exception),
    .io_insts_pack_PD_1_inst         (_pd_io_insts_pack_PD_1_inst),
    .io_pred_fix                     (_pd_io_pred_fix),
    .io_pred_fix_target              (_pc_io_flush_pd_target),
    .io_pred_fix_is_bl               (_predict_io_pd_pred_fix_is_bl),
    .io_pred_fix_pc                  (_predict_io_pd_pc_plus_4)
  );
  assign _fq_io_next_ready = ~_fq_io_next_ready_T_1;
  assign _fq_io_flush = _rob_io_predict_fail_cmt[3];
  Fetch_Queue fq (
    .clock                           (clock),
    .reset                           (reset),
    .io_insts_pack_0_pc              (_pd_io_insts_pack_PD_0_pc),
    .io_insts_pack_0_inst            (fq_io_insts_pack_inst_pack_IF_inst),
    .io_insts_pack_0_inst_valid      (_pd_io_insts_pack_PD_0_inst_valid),
    .io_insts_pack_0_predict_jump    (_pd_io_insts_pack_PD_0_predict_jump),
    .io_insts_pack_0_pred_npc        (_pd_io_insts_pack_PD_0_pred_npc),
    .io_insts_pack_0_exception       (fq_io_insts_pack_inst_pack_IF_exception),
    .io_insts_pack_1_pc              (_pd_io_insts_pack_PD_1_pc),
    .io_insts_pack_1_inst            (fq_io_insts_pack_inst_pack_IF_1_inst),
    .io_insts_pack_1_inst_valid      (_pd_io_insts_pack_PD_1_inst_valid),
    .io_insts_pack_1_predict_jump    (_pd_io_insts_pack_PD_1_predict_jump),
    .io_insts_pack_1_pred_npc        (_pd_io_insts_pack_PD_1_pred_npc),
    .io_insts_pack_1_exception       (fq_io_insts_pack_inst_pack_IF_1_exception),
    .io_next_ready                   (_fq_io_next_ready),
    .io_flush                        (_fq_io_flush),
    .io_insts_valid_decode_0         (_fq_io_insts_valid_decode_0),
    .io_insts_valid_decode_1         (_fq_io_insts_valid_decode_1),
    .io_insts_pack_id_0_pc           (_fq_io_insts_pack_id_0_pc),
    .io_insts_pack_id_0_inst         (_fq_io_insts_pack_id_0_inst),
    .io_insts_pack_id_0_predict_jump (_fq_io_insts_pack_id_0_predict_jump),
    .io_insts_pack_id_0_pred_npc     (_fq_io_insts_pack_id_0_pred_npc),
    .io_insts_pack_id_0_exception    (_fq_io_insts_pack_id_0_exception),
    .io_insts_pack_id_1_pc           (_fq_io_insts_pack_id_1_pc),
    .io_insts_pack_id_1_inst         (_fq_io_insts_pack_id_1_inst),
    .io_insts_pack_id_1_predict_jump (_fq_io_insts_pack_id_1_predict_jump),
    .io_insts_pack_id_1_pred_npc     (_fq_io_insts_pack_id_1_pred_npc),
    .io_insts_pack_id_1_exception    (_fq_io_insts_pack_id_1_exception),
    .io_full                         (_fq_io_full)
  );
  Decode Decode (
    .io_inst        (_fq_io_insts_pack_id_0_inst),
    .io_rj          (_Decode_io_rj),
    .io_rk          (_Decode_io_rk),
    .io_rd          (_Decode_io_rd),
    .io_rd_valid    (_Decode_io_rd_valid),
    .io_imm         (_Decode_io_imm),
    .io_alu_op      (_Decode_io_alu_op),
    .io_alu_rs1_sel (_Decode_io_alu_rs1_sel),
    .io_alu_rs2_sel (_Decode_io_alu_rs2_sel),
    .io_br_type     (_Decode_io_br_type),
    .io_mem_type    (_Decode_io_mem_type),
    .io_priv_vec    (_Decode_io_priv_vec),
    .io_fu_id       (_Decode_io_fu_id),
    .io_exception   (_Decode_io_exception)
  );
  Decode Decode_1 (
    .io_inst        (_fq_io_insts_pack_id_1_inst),
    .io_rj          (_Decode_1_io_rj),
    .io_rk          (_Decode_1_io_rk),
    .io_rd          (_Decode_1_io_rd),
    .io_rd_valid    (_Decode_1_io_rd_valid),
    .io_imm         (_Decode_1_io_imm),
    .io_alu_op      (_Decode_1_io_alu_op),
    .io_alu_rs1_sel (_Decode_1_io_alu_rs1_sel),
    .io_alu_rs2_sel (_Decode_1_io_alu_rs2_sel),
    .io_br_type     (_Decode_1_io_br_type),
    .io_mem_type    (_Decode_1_io_mem_type),
    .io_priv_vec    (_Decode_1_io_priv_vec),
    .io_fu_id       (_Decode_1_io_fu_id),
    .io_exception   (_Decode_1_io_exception)
  );
  assign _dr_reg_io_flush =
    _rob_io_predict_fail_cmt[4] | ~_dr_reg_io_stall_T & _free_list_io_empty;
  assign _dr_reg_io_insts_pack_ID_0_alu_op = _Decode_io_alu_op[3:0];
  assign _dr_reg_io_insts_pack_ID_1_alu_op = _Decode_1_io_alu_op[3:0];
  ID_RN_Reg dr_reg (
    .clock                           (clock),
    .reset                           (reset),
    .io_flush                        (_dr_reg_io_flush),
    .io_stall                        (_dr_reg_io_stall_T),
    .io_insts_pack_ID_0_pc           (_fq_io_insts_pack_id_0_pc),
    .io_insts_pack_ID_0_inst_valid   (_fq_io_insts_valid_decode_0),
    .io_insts_pack_ID_0_predict_jump (_fq_io_insts_pack_id_0_predict_jump),
    .io_insts_pack_ID_0_pred_npc     (_fq_io_insts_pack_id_0_pred_npc),
    .io_insts_pack_ID_0_exception    (inst_pack_ID_exception),
    .io_insts_pack_ID_0_rj           (_Decode_io_rj),
    .io_insts_pack_ID_0_rk           (_Decode_io_rk),
    .io_insts_pack_ID_0_rd           (_Decode_io_rd),
    .io_insts_pack_ID_0_rd_valid     (_Decode_io_rd_valid),
    .io_insts_pack_ID_0_imm          (_Decode_io_imm),
    .io_insts_pack_ID_0_alu_op       (_dr_reg_io_insts_pack_ID_0_alu_op),
    .io_insts_pack_ID_0_alu_rs1_sel  (_Decode_io_alu_rs1_sel),
    .io_insts_pack_ID_0_alu_rs2_sel  (_Decode_io_alu_rs2_sel),
    .io_insts_pack_ID_0_br_type      (_Decode_io_br_type),
    .io_insts_pack_ID_0_mem_type     (_Decode_io_mem_type),
    .io_insts_pack_ID_0_priv_vec     (_Decode_io_priv_vec),
    .io_insts_pack_ID_0_fu_id        (inst_pack_ID_fu_id),
    .io_insts_pack_ID_1_pc           (_fq_io_insts_pack_id_1_pc),
    .io_insts_pack_ID_1_inst_valid   (_fq_io_insts_valid_decode_1),
    .io_insts_pack_ID_1_predict_jump (_fq_io_insts_pack_id_1_predict_jump),
    .io_insts_pack_ID_1_pred_npc     (_fq_io_insts_pack_id_1_pred_npc),
    .io_insts_pack_ID_1_exception    (inst_pack_ID_1_exception),
    .io_insts_pack_ID_1_rj           (_Decode_1_io_rj),
    .io_insts_pack_ID_1_rk           (_Decode_1_io_rk),
    .io_insts_pack_ID_1_rd           (_Decode_1_io_rd),
    .io_insts_pack_ID_1_rd_valid     (_Decode_1_io_rd_valid),
    .io_insts_pack_ID_1_imm          (_Decode_1_io_imm),
    .io_insts_pack_ID_1_alu_op       (_dr_reg_io_insts_pack_ID_1_alu_op),
    .io_insts_pack_ID_1_alu_rs1_sel  (_Decode_1_io_alu_rs1_sel),
    .io_insts_pack_ID_1_alu_rs2_sel  (_Decode_1_io_alu_rs2_sel),
    .io_insts_pack_ID_1_br_type      (_Decode_1_io_br_type),
    .io_insts_pack_ID_1_mem_type     (_Decode_1_io_mem_type),
    .io_insts_pack_ID_1_priv_vec     (_Decode_1_io_priv_vec),
    .io_insts_pack_ID_1_fu_id        (inst_pack_ID_1_fu_id),
    .io_alloc_preg_ID_0              (_dr_reg_io_alloc_preg_ID_0),
    .io_alloc_preg_ID_1              (_dr_reg_io_alloc_preg_ID_1),
    .io_inst_ID_0                    (_fq_io_insts_pack_id_0_inst),
    .io_inst_ID_1                    (_fq_io_insts_pack_id_1_inst),
    .io_insts_pack_RN_0_pc           (_dr_reg_io_insts_pack_RN_0_pc),
    .io_insts_pack_RN_0_inst_valid   (_dr_reg_io_insts_pack_RN_0_inst_valid),
    .io_insts_pack_RN_0_predict_jump (_dr_reg_io_insts_pack_RN_0_predict_jump),
    .io_insts_pack_RN_0_pred_npc     (_dr_reg_io_insts_pack_RN_0_pred_npc),
    .io_insts_pack_RN_0_exception    (_dr_reg_io_insts_pack_RN_0_exception),
    .io_insts_pack_RN_0_rj           (_dr_reg_io_insts_pack_RN_0_rj),
    .io_insts_pack_RN_0_rk           (_dr_reg_io_insts_pack_RN_0_rk),
    .io_insts_pack_RN_0_rd           (_dr_reg_io_insts_pack_RN_0_rd),
    .io_insts_pack_RN_0_rd_valid     (_dr_reg_io_insts_pack_RN_0_rd_valid),
    .io_insts_pack_RN_0_imm          (_dr_reg_io_insts_pack_RN_0_imm),
    .io_insts_pack_RN_0_alu_op       (_dr_reg_io_insts_pack_RN_0_alu_op),
    .io_insts_pack_RN_0_alu_rs1_sel  (_dr_reg_io_insts_pack_RN_0_alu_rs1_sel),
    .io_insts_pack_RN_0_alu_rs2_sel  (_dr_reg_io_insts_pack_RN_0_alu_rs2_sel),
    .io_insts_pack_RN_0_br_type      (_dr_reg_io_insts_pack_RN_0_br_type),
    .io_insts_pack_RN_0_mem_type     (_dr_reg_io_insts_pack_RN_0_mem_type),
    .io_insts_pack_RN_0_priv_vec     (_dr_reg_io_insts_pack_RN_0_priv_vec),
    .io_insts_pack_RN_0_fu_id        (_dr_reg_io_insts_pack_RN_0_fu_id),
    .io_insts_pack_RN_1_pc           (_dr_reg_io_insts_pack_RN_1_pc),
    .io_insts_pack_RN_1_inst_valid   (_dr_reg_io_insts_pack_RN_1_inst_valid),
    .io_insts_pack_RN_1_predict_jump (_dr_reg_io_insts_pack_RN_1_predict_jump),
    .io_insts_pack_RN_1_pred_npc     (_dr_reg_io_insts_pack_RN_1_pred_npc),
    .io_insts_pack_RN_1_exception    (_dr_reg_io_insts_pack_RN_1_exception),
    .io_insts_pack_RN_1_rj           (_dr_reg_io_insts_pack_RN_1_rj),
    .io_insts_pack_RN_1_rk           (_dr_reg_io_insts_pack_RN_1_rk),
    .io_insts_pack_RN_1_rd           (_dr_reg_io_insts_pack_RN_1_rd),
    .io_insts_pack_RN_1_rd_valid     (_dr_reg_io_insts_pack_RN_1_rd_valid),
    .io_insts_pack_RN_1_imm          (_dr_reg_io_insts_pack_RN_1_imm),
    .io_insts_pack_RN_1_alu_op       (_dr_reg_io_insts_pack_RN_1_alu_op),
    .io_insts_pack_RN_1_alu_rs1_sel  (_dr_reg_io_insts_pack_RN_1_alu_rs1_sel),
    .io_insts_pack_RN_1_alu_rs2_sel  (_dr_reg_io_insts_pack_RN_1_alu_rs2_sel),
    .io_insts_pack_RN_1_br_type      (_dr_reg_io_insts_pack_RN_1_br_type),
    .io_insts_pack_RN_1_mem_type     (_dr_reg_io_insts_pack_RN_1_mem_type),
    .io_insts_pack_RN_1_priv_vec     (_dr_reg_io_insts_pack_RN_1_priv_vec),
    .io_insts_pack_RN_1_fu_id        (_dr_reg_io_insts_pack_RN_1_fu_id),
    .io_alloc_preg_RN_0              (_dr_reg_io_alloc_preg_RN_0),
    .io_alloc_preg_RN_1              (_dr_reg_io_alloc_preg_RN_1),
    .io_inst_RN_0                    (_dr_reg_io_inst_RN_0),
    .io_inst_RN_1                    (_dr_reg_io_inst_RN_1)
  );
  assign _free_list_io_rename_en_0 = _fq_io_insts_valid_decode_0 & ~_fq_io_next_ready_T_1;
  assign _free_list_io_rename_en_1 = _fq_io_insts_valid_decode_1 & ~_fq_io_next_ready_T_1;
  assign _free_list_io_commit_en_0 = _rob_io_cmt_en_0;
  assign _free_list_io_commit_en_1 = _rob_io_cmt_en_1;
  assign _free_list_io_commit_pprd_valid_0 =
    _rob_io_rd_valid_cmt_0 & (|_rob_io_pprd_cmt_0);
  assign _free_list_io_commit_pprd_valid_1 =
    _rob_io_rd_valid_cmt_1 & (|_rob_io_pprd_cmt_1);
  assign _free_list_io_commit_pprd_0 = _rob_io_pprd_cmt_0;
  assign _free_list_io_commit_pprd_1 = _rob_io_pprd_cmt_1;
  Free_List free_list (
    .clock                  (clock),
    .reset                  (reset),
    .io_rd_valid_0          (_Decode_io_rd_valid),
    .io_rd_valid_1          (_Decode_1_io_rd_valid),
    .io_rename_en_0         (_free_list_io_rename_en_0),
    .io_rename_en_1         (_free_list_io_rename_en_1),
    .io_commit_en_0         (_free_list_io_commit_en_0),
    .io_commit_en_1         (_free_list_io_commit_en_1),
    .io_commit_pprd_valid_0 (_free_list_io_commit_pprd_valid_0),
    .io_commit_pprd_valid_1 (_free_list_io_commit_pprd_valid_1),
    .io_commit_pprd_0       (_free_list_io_commit_pprd_0),
    .io_commit_pprd_1       (_free_list_io_commit_pprd_1),
    .io_predict_fail        (free_list_io_predict_fail_r),
    .io_head_arch           (_free_list_io_head_arch),
    .io_alloc_preg_0        (_dr_reg_io_alloc_preg_ID_0),
    .io_alloc_preg_1        (_dr_reg_io_alloc_preg_ID_1),
    .io_empty               (_free_list_io_empty)
  );
  assign _rename_io_rename_en_0 =
    _dr_reg_io_insts_pack_RN_0_inst_valid & ~_dr_reg_io_stall_T;
  assign _rename_io_rename_en_1 =
    _dr_reg_io_insts_pack_RN_1_inst_valid & ~_dr_reg_io_stall_T;
  assign _rename_io_prd_wake_0 = _sel1_io_wake_preg;
  assign _rename_io_prd_wake_1 = _sel2_io_wake_preg;
  assign _rename_io_prd_wake_2 = _md_ex2_ex3_reg_io_inst_pack_EX2_prd;
  assign _rename_io_prd_wake_3 = _re_reg4_io_inst_pack_EX_prd;
  assign _rename_io_wake_valid_0 = _sel1_io_inst_issue_valid;
  assign _rename_io_wake_valid_1 = _sel2_io_inst_issue_valid;
  assign _rename_io_wake_valid_2 = ~_mdu_io_busy_16;
  assign _rename_io_wake_valid_3 = ~_dcache_io_cache_miss_MEM_4;
  Reg_Rename rename (
    .clock           (clock),
    .reset           (reset),
    .io_rj_0         (_dr_reg_io_insts_pack_RN_0_rj),
    .io_rj_1         (_dr_reg_io_insts_pack_RN_1_rj),
    .io_rk_0         (_dr_reg_io_insts_pack_RN_0_rk),
    .io_rk_1         (_dr_reg_io_insts_pack_RN_1_rk),
    .io_rd_0         (_dr_reg_io_insts_pack_RN_0_rd),
    .io_rd_1         (_dr_reg_io_insts_pack_RN_1_rd),
    .io_rd_valid_0   (_dr_reg_io_insts_pack_RN_0_rd_valid),
    .io_rd_valid_1   (_dr_reg_io_insts_pack_RN_1_rd_valid),
    .io_rename_en_0  (_rename_io_rename_en_0),
    .io_rename_en_1  (_rename_io_rename_en_1),
    .io_alloc_preg_0 (_dr_reg_io_alloc_preg_RN_0),
    .io_alloc_preg_1 (_dr_reg_io_alloc_preg_RN_1),
    .io_prj_0        (_rename_io_prj_0),
    .io_prj_1        (_rename_io_prj_1),
    .io_prk_0        (_rename_io_prk_0),
    .io_prk_1        (_rename_io_prk_1),
    .io_prd_0        (_rename_io_prd_0),
    .io_prd_1        (_rename_io_prd_1),
    .io_pprd_0       (_rename_io_pprd_0),
    .io_pprd_1       (_rename_io_pprd_1),
    .io_predict_fail (rename_io_predict_fail_r),
    .io_arch_rat_0   (_rename_io_arch_rat_0),
    .io_arch_rat_1   (_rename_io_arch_rat_1),
    .io_arch_rat_2   (_rename_io_arch_rat_2),
    .io_arch_rat_3   (_rename_io_arch_rat_3),
    .io_arch_rat_4   (_rename_io_arch_rat_4),
    .io_arch_rat_5   (_rename_io_arch_rat_5),
    .io_arch_rat_6   (_rename_io_arch_rat_6),
    .io_arch_rat_7   (_rename_io_arch_rat_7),
    .io_arch_rat_8   (_rename_io_arch_rat_8),
    .io_arch_rat_9   (_rename_io_arch_rat_9),
    .io_arch_rat_10  (_rename_io_arch_rat_10),
    .io_arch_rat_11  (_rename_io_arch_rat_11),
    .io_arch_rat_12  (_rename_io_arch_rat_12),
    .io_arch_rat_13  (_rename_io_arch_rat_13),
    .io_arch_rat_14  (_rename_io_arch_rat_14),
    .io_arch_rat_15  (_rename_io_arch_rat_15),
    .io_arch_rat_16  (_rename_io_arch_rat_16),
    .io_arch_rat_17  (_rename_io_arch_rat_17),
    .io_arch_rat_18  (_rename_io_arch_rat_18),
    .io_arch_rat_19  (_rename_io_arch_rat_19),
    .io_arch_rat_20  (_rename_io_arch_rat_20),
    .io_arch_rat_21  (_rename_io_arch_rat_21),
    .io_arch_rat_22  (_rename_io_arch_rat_22),
    .io_arch_rat_23  (_rename_io_arch_rat_23),
    .io_arch_rat_24  (_rename_io_arch_rat_24),
    .io_arch_rat_25  (_rename_io_arch_rat_25),
    .io_arch_rat_26  (_rename_io_arch_rat_26),
    .io_arch_rat_27  (_rename_io_arch_rat_27),
    .io_arch_rat_28  (_rename_io_arch_rat_28),
    .io_arch_rat_29  (_rename_io_arch_rat_29),
    .io_arch_rat_30  (_rename_io_arch_rat_30),
    .io_arch_rat_31  (_rename_io_arch_rat_31),
    .io_arch_rat_32  (_rename_io_arch_rat_32),
    .io_arch_rat_33  (_rename_io_arch_rat_33),
    .io_arch_rat_34  (_rename_io_arch_rat_34),
    .io_arch_rat_35  (_rename_io_arch_rat_35),
    .io_arch_rat_36  (_rename_io_arch_rat_36),
    .io_arch_rat_37  (_rename_io_arch_rat_37),
    .io_arch_rat_38  (_rename_io_arch_rat_38),
    .io_arch_rat_39  (_rename_io_arch_rat_39),
    .io_arch_rat_40  (_rename_io_arch_rat_40),
    .io_arch_rat_41  (_rename_io_arch_rat_41),
    .io_arch_rat_42  (_rename_io_arch_rat_42),
    .io_arch_rat_43  (_rename_io_arch_rat_43),
    .io_arch_rat_44  (_rename_io_arch_rat_44),
    .io_arch_rat_45  (_rename_io_arch_rat_45),
    .io_arch_rat_46  (_rename_io_arch_rat_46),
    .io_arch_rat_47  (_rename_io_arch_rat_47),
    .io_arch_rat_48  (_rename_io_arch_rat_48),
    .io_arch_rat_49  (_rename_io_arch_rat_49),
    .io_arch_rat_50  (_rename_io_arch_rat_50),
    .io_arch_rat_51  (_rename_io_arch_rat_51),
    .io_arch_rat_52  (_rename_io_arch_rat_52),
    .io_arch_rat_53  (_rename_io_arch_rat_53),
    .io_arch_rat_54  (_rename_io_arch_rat_54),
    .io_arch_rat_55  (_rename_io_arch_rat_55),
    .io_arch_rat_56  (_rename_io_arch_rat_56),
    .io_arch_rat_57  (_rename_io_arch_rat_57),
    .io_arch_rat_58  (_rename_io_arch_rat_58),
    .io_arch_rat_59  (_rename_io_arch_rat_59),
    .io_arch_rat_60  (_rename_io_arch_rat_60),
    .io_arch_rat_61  (_rename_io_arch_rat_61),
    .io_arch_rat_62  (_rename_io_arch_rat_62),
    .io_arch_rat_63  (_rename_io_arch_rat_63),
    .io_prj_ready_0  (_rename_io_prj_ready_0),
    .io_prj_ready_1  (_rename_io_prj_ready_1),
    .io_prk_ready_0  (_rename_io_prk_ready_0),
    .io_prk_ready_1  (_rename_io_prk_ready_1),
    .io_prd_wake_0   (_rename_io_prd_wake_0),
    .io_prd_wake_1   (_rename_io_prd_wake_1),
    .io_prd_wake_2   (_rename_io_prd_wake_2),
    .io_prd_wake_3   (_rename_io_prd_wake_3),
    .io_wake_valid_0 (_rename_io_wake_valid_0),
    .io_wake_valid_1 (_rename_io_wake_valid_1),
    .io_wake_valid_2 (_rename_io_wake_valid_2),
    .io_wake_valid_3 (_rename_io_wake_valid_3)
  );
  Dispatch dp (
    .io_inst_packs_0_inst_valid (_dr_reg_io_insts_pack_RN_0_inst_valid),
    .io_inst_packs_0_fu_id      (_dr_reg_io_insts_pack_RN_0_fu_id),
    .io_inst_packs_1_inst_valid (_dr_reg_io_insts_pack_RN_1_inst_valid),
    .io_inst_packs_1_fu_id      (_dr_reg_io_insts_pack_RN_1_fu_id),
    .io_elem_num_0              (_dp_io_elem_num_0),
    .io_elem_num_1              (_dp_io_elem_num_1),
    .io_insts_disp_valid_0_0    (_dp_io_insts_disp_valid_0_0),
    .io_insts_disp_valid_0_1    (_dp_io_insts_disp_valid_0_1),
    .io_insts_disp_valid_1_0    (_dp_io_insts_disp_valid_1_0),
    .io_insts_disp_valid_1_1    (_dp_io_insts_disp_valid_1_1),
    .io_insts_disp_valid_2_0    (_dp_io_insts_disp_valid_2_0),
    .io_insts_disp_valid_2_1    (_dp_io_insts_disp_valid_2_1),
    .io_insts_disp_valid_3_0    (_dp_io_insts_disp_valid_3_0),
    .io_insts_disp_valid_3_1    (_dp_io_insts_disp_valid_3_1)
  );
  assign _iq1_io_insts_dispatch_0_rob_index = _rob_io_rob_index_dp_0;
  assign _iq1_io_insts_dispatch_1_rob_index = _rob_io_rob_index_dp_1;
  assign _iq1_io_wake_preg_0 = _sel1_io_wake_preg;
  assign _iq1_io_wake_preg_1 = _sel2_io_wake_preg;
  assign _iq1_io_ld_mem_prd = _ls_ex_mem_reg_io_prd_MEM_0[5:0];
  assign _iq1_io_flush = _rob_io_predict_fail_cmt[6];
  Unorder_Issue_Queue iq1 (
    .clock                             (clock),
    .reset                             (reset),
    .io_insts_disp_valid_0             (_dp_io_insts_disp_valid_0_0),
    .io_insts_disp_valid_1             (_dp_io_insts_disp_valid_0_1),
    .io_insts_dispatch_0_prj           (_rename_io_prj_0),
    .io_insts_dispatch_0_prk           (_rename_io_prk_0),
    .io_insts_dispatch_0_rd_valid      (_dr_reg_io_insts_pack_RN_0_rd_valid),
    .io_insts_dispatch_0_prd           (inst_pack_RN_prd),
    .io_insts_dispatch_0_imm           (_dr_reg_io_insts_pack_RN_0_imm),
    .io_insts_dispatch_0_rob_index     (_iq1_io_insts_dispatch_0_rob_index),
    .io_insts_dispatch_0_alu_op        (_dr_reg_io_insts_pack_RN_0_alu_op),
    .io_insts_dispatch_0_alu_rs1_sel   (_dr_reg_io_insts_pack_RN_0_alu_rs1_sel),
    .io_insts_dispatch_0_alu_rs2_sel   (_dr_reg_io_insts_pack_RN_0_alu_rs2_sel),
    .io_insts_dispatch_0_pc            (_dr_reg_io_insts_pack_RN_0_pc),
    .io_insts_dispatch_1_prj           (_rename_io_prj_1),
    .io_insts_dispatch_1_prk           (_rename_io_prk_1),
    .io_insts_dispatch_1_rd_valid      (_dr_reg_io_insts_pack_RN_1_rd_valid),
    .io_insts_dispatch_1_prd           (inst_pack_RN_1_prd),
    .io_insts_dispatch_1_imm           (_dr_reg_io_insts_pack_RN_1_imm),
    .io_insts_dispatch_1_rob_index     (_iq1_io_insts_dispatch_1_rob_index),
    .io_insts_dispatch_1_alu_op        (_dr_reg_io_insts_pack_RN_1_alu_op),
    .io_insts_dispatch_1_alu_rs1_sel   (_dr_reg_io_insts_pack_RN_1_alu_rs1_sel),
    .io_insts_dispatch_1_alu_rs2_sel   (_dr_reg_io_insts_pack_RN_1_alu_rs2_sel),
    .io_insts_dispatch_1_pc            (_dr_reg_io_insts_pack_RN_1_pc),
    .io_prj_ready_0                    (_rename_io_prj_ready_0),
    .io_prj_ready_1                    (_rename_io_prj_ready_1),
    .io_prk_ready_0                    (_rename_io_prk_ready_0),
    .io_prk_ready_1                    (_rename_io_prk_ready_1),
    .io_wake_preg_0                    (_iq1_io_wake_preg_0),
    .io_wake_preg_1                    (_iq1_io_wake_preg_1),
    .io_wake_preg_2                    (iq_mutual_wake_preg_2),
    .io_wake_preg_3                    (iq_mutual_wake_preg_3),
    .io_ld_mem_prd                     (_iq1_io_ld_mem_prd),
    .io_issue_ack_0                    (_iq1_io_issue_ack_0),
    .io_issue_ack_1                    (_iq1_io_issue_ack_1),
    .io_issue_ack_2                    (_iq1_io_issue_ack_2),
    .io_issue_ack_3                    (_iq1_io_issue_ack_3),
    .io_issue_ack_4                    (_iq1_io_issue_ack_4),
    .io_issue_ack_5                    (_iq1_io_issue_ack_5),
    .io_insts_issue_0_inst_prj         (_iq1_io_insts_issue_0_inst_prj),
    .io_insts_issue_0_inst_prk         (_iq1_io_insts_issue_0_inst_prk),
    .io_insts_issue_0_inst_rd_valid    (_iq1_io_insts_issue_0_inst_rd_valid),
    .io_insts_issue_0_inst_prd         (_iq1_io_insts_issue_0_inst_prd),
    .io_insts_issue_0_inst_imm         (_iq1_io_insts_issue_0_inst_imm),
    .io_insts_issue_0_inst_rob_index   (_iq1_io_insts_issue_0_inst_rob_index),
    .io_insts_issue_0_inst_alu_op      (_iq1_io_insts_issue_0_inst_alu_op),
    .io_insts_issue_0_inst_alu_rs1_sel (_iq1_io_insts_issue_0_inst_alu_rs1_sel),
    .io_insts_issue_0_inst_alu_rs2_sel (_iq1_io_insts_issue_0_inst_alu_rs2_sel),
    .io_insts_issue_0_inst_pc          (_iq1_io_insts_issue_0_inst_pc),
    .io_insts_issue_1_inst_prj         (_iq1_io_insts_issue_1_inst_prj),
    .io_insts_issue_1_inst_prk         (_iq1_io_insts_issue_1_inst_prk),
    .io_insts_issue_1_inst_rd_valid    (_iq1_io_insts_issue_1_inst_rd_valid),
    .io_insts_issue_1_inst_prd         (_iq1_io_insts_issue_1_inst_prd),
    .io_insts_issue_1_inst_imm         (_iq1_io_insts_issue_1_inst_imm),
    .io_insts_issue_1_inst_rob_index   (_iq1_io_insts_issue_1_inst_rob_index),
    .io_insts_issue_1_inst_alu_op      (_iq1_io_insts_issue_1_inst_alu_op),
    .io_insts_issue_1_inst_alu_rs1_sel (_iq1_io_insts_issue_1_inst_alu_rs1_sel),
    .io_insts_issue_1_inst_alu_rs2_sel (_iq1_io_insts_issue_1_inst_alu_rs2_sel),
    .io_insts_issue_1_inst_pc          (_iq1_io_insts_issue_1_inst_pc),
    .io_insts_issue_2_inst_prj         (_iq1_io_insts_issue_2_inst_prj),
    .io_insts_issue_2_inst_prk         (_iq1_io_insts_issue_2_inst_prk),
    .io_insts_issue_2_inst_rd_valid    (_iq1_io_insts_issue_2_inst_rd_valid),
    .io_insts_issue_2_inst_prd         (_iq1_io_insts_issue_2_inst_prd),
    .io_insts_issue_2_inst_imm         (_iq1_io_insts_issue_2_inst_imm),
    .io_insts_issue_2_inst_rob_index   (_iq1_io_insts_issue_2_inst_rob_index),
    .io_insts_issue_2_inst_alu_op      (_iq1_io_insts_issue_2_inst_alu_op),
    .io_insts_issue_2_inst_alu_rs1_sel (_iq1_io_insts_issue_2_inst_alu_rs1_sel),
    .io_insts_issue_2_inst_alu_rs2_sel (_iq1_io_insts_issue_2_inst_alu_rs2_sel),
    .io_insts_issue_2_inst_pc          (_iq1_io_insts_issue_2_inst_pc),
    .io_insts_issue_3_inst_prj         (_iq1_io_insts_issue_3_inst_prj),
    .io_insts_issue_3_inst_prk         (_iq1_io_insts_issue_3_inst_prk),
    .io_insts_issue_3_inst_rd_valid    (_iq1_io_insts_issue_3_inst_rd_valid),
    .io_insts_issue_3_inst_prd         (_iq1_io_insts_issue_3_inst_prd),
    .io_insts_issue_3_inst_imm         (_iq1_io_insts_issue_3_inst_imm),
    .io_insts_issue_3_inst_rob_index   (_iq1_io_insts_issue_3_inst_rob_index),
    .io_insts_issue_3_inst_alu_op      (_iq1_io_insts_issue_3_inst_alu_op),
    .io_insts_issue_3_inst_alu_rs1_sel (_iq1_io_insts_issue_3_inst_alu_rs1_sel),
    .io_insts_issue_3_inst_alu_rs2_sel (_iq1_io_insts_issue_3_inst_alu_rs2_sel),
    .io_insts_issue_3_inst_pc          (_iq1_io_insts_issue_3_inst_pc),
    .io_insts_issue_4_inst_prj         (_iq1_io_insts_issue_4_inst_prj),
    .io_insts_issue_4_inst_prk         (_iq1_io_insts_issue_4_inst_prk),
    .io_insts_issue_4_inst_rd_valid    (_iq1_io_insts_issue_4_inst_rd_valid),
    .io_insts_issue_4_inst_prd         (_iq1_io_insts_issue_4_inst_prd),
    .io_insts_issue_4_inst_imm         (_iq1_io_insts_issue_4_inst_imm),
    .io_insts_issue_4_inst_rob_index   (_iq1_io_insts_issue_4_inst_rob_index),
    .io_insts_issue_4_inst_alu_op      (_iq1_io_insts_issue_4_inst_alu_op),
    .io_insts_issue_4_inst_alu_rs1_sel (_iq1_io_insts_issue_4_inst_alu_rs1_sel),
    .io_insts_issue_4_inst_alu_rs2_sel (_iq1_io_insts_issue_4_inst_alu_rs2_sel),
    .io_insts_issue_4_inst_pc          (_iq1_io_insts_issue_4_inst_pc),
    .io_insts_issue_5_inst_prj         (_iq1_io_insts_issue_5_inst_prj),
    .io_insts_issue_5_inst_prk         (_iq1_io_insts_issue_5_inst_prk),
    .io_insts_issue_5_inst_rd_valid    (_iq1_io_insts_issue_5_inst_rd_valid),
    .io_insts_issue_5_inst_prd         (_iq1_io_insts_issue_5_inst_prd),
    .io_insts_issue_5_inst_imm         (_iq1_io_insts_issue_5_inst_imm),
    .io_insts_issue_5_inst_rob_index   (_iq1_io_insts_issue_5_inst_rob_index),
    .io_insts_issue_5_inst_alu_op      (_iq1_io_insts_issue_5_inst_alu_op),
    .io_insts_issue_5_inst_alu_rs1_sel (_iq1_io_insts_issue_5_inst_alu_rs1_sel),
    .io_insts_issue_5_inst_alu_rs2_sel (_iq1_io_insts_issue_5_inst_alu_rs2_sel),
    .io_insts_issue_5_inst_pc          (_iq1_io_insts_issue_5_inst_pc),
    .io_issue_req_0                    (_iq1_io_issue_req_0),
    .io_issue_req_1                    (_iq1_io_issue_req_1),
    .io_issue_req_2                    (_iq1_io_issue_req_2),
    .io_issue_req_3                    (_iq1_io_issue_req_3),
    .io_issue_req_4                    (_iq1_io_issue_req_4),
    .io_issue_req_5                    (_iq1_io_issue_req_5),
    .io_elem_num                       (_dp_io_elem_num_0),
    .io_full                           (_iq1_io_full),
    .io_stall                          (_iq4_io_stall_T),
    .io_flush                          (_iq1_io_flush),
    .io_dcache_miss                    (_iq1_io_dcache_miss)
  );
  assign _sel1_io_stall =
    {_iq1_io_issue_req_5,
     _iq1_io_issue_req_4,
     _iq1_io_issue_req_3,
     _iq1_io_issue_req_2,
     _iq1_io_issue_req_1,
     _iq1_io_issue_req_0} == 6'h0;
  Unorder_Select sel1 (
    .io_insts_issue_0_inst_prj         (_iq1_io_insts_issue_0_inst_prj),
    .io_insts_issue_0_inst_prk         (_iq1_io_insts_issue_0_inst_prk),
    .io_insts_issue_0_inst_rd_valid    (_iq1_io_insts_issue_0_inst_rd_valid),
    .io_insts_issue_0_inst_prd         (_iq1_io_insts_issue_0_inst_prd),
    .io_insts_issue_0_inst_imm         (_iq1_io_insts_issue_0_inst_imm),
    .io_insts_issue_0_inst_rob_index   (_iq1_io_insts_issue_0_inst_rob_index),
    .io_insts_issue_0_inst_alu_op      (_iq1_io_insts_issue_0_inst_alu_op),
    .io_insts_issue_0_inst_alu_rs1_sel (_iq1_io_insts_issue_0_inst_alu_rs1_sel),
    .io_insts_issue_0_inst_alu_rs2_sel (_iq1_io_insts_issue_0_inst_alu_rs2_sel),
    .io_insts_issue_0_inst_pc          (_iq1_io_insts_issue_0_inst_pc),
    .io_insts_issue_1_inst_prj         (_iq1_io_insts_issue_1_inst_prj),
    .io_insts_issue_1_inst_prk         (_iq1_io_insts_issue_1_inst_prk),
    .io_insts_issue_1_inst_rd_valid    (_iq1_io_insts_issue_1_inst_rd_valid),
    .io_insts_issue_1_inst_prd         (_iq1_io_insts_issue_1_inst_prd),
    .io_insts_issue_1_inst_imm         (_iq1_io_insts_issue_1_inst_imm),
    .io_insts_issue_1_inst_rob_index   (_iq1_io_insts_issue_1_inst_rob_index),
    .io_insts_issue_1_inst_alu_op      (_iq1_io_insts_issue_1_inst_alu_op),
    .io_insts_issue_1_inst_alu_rs1_sel (_iq1_io_insts_issue_1_inst_alu_rs1_sel),
    .io_insts_issue_1_inst_alu_rs2_sel (_iq1_io_insts_issue_1_inst_alu_rs2_sel),
    .io_insts_issue_1_inst_pc          (_iq1_io_insts_issue_1_inst_pc),
    .io_insts_issue_2_inst_prj         (_iq1_io_insts_issue_2_inst_prj),
    .io_insts_issue_2_inst_prk         (_iq1_io_insts_issue_2_inst_prk),
    .io_insts_issue_2_inst_rd_valid    (_iq1_io_insts_issue_2_inst_rd_valid),
    .io_insts_issue_2_inst_prd         (_iq1_io_insts_issue_2_inst_prd),
    .io_insts_issue_2_inst_imm         (_iq1_io_insts_issue_2_inst_imm),
    .io_insts_issue_2_inst_rob_index   (_iq1_io_insts_issue_2_inst_rob_index),
    .io_insts_issue_2_inst_alu_op      (_iq1_io_insts_issue_2_inst_alu_op),
    .io_insts_issue_2_inst_alu_rs1_sel (_iq1_io_insts_issue_2_inst_alu_rs1_sel),
    .io_insts_issue_2_inst_alu_rs2_sel (_iq1_io_insts_issue_2_inst_alu_rs2_sel),
    .io_insts_issue_2_inst_pc          (_iq1_io_insts_issue_2_inst_pc),
    .io_insts_issue_3_inst_prj         (_iq1_io_insts_issue_3_inst_prj),
    .io_insts_issue_3_inst_prk         (_iq1_io_insts_issue_3_inst_prk),
    .io_insts_issue_3_inst_rd_valid    (_iq1_io_insts_issue_3_inst_rd_valid),
    .io_insts_issue_3_inst_prd         (_iq1_io_insts_issue_3_inst_prd),
    .io_insts_issue_3_inst_imm         (_iq1_io_insts_issue_3_inst_imm),
    .io_insts_issue_3_inst_rob_index   (_iq1_io_insts_issue_3_inst_rob_index),
    .io_insts_issue_3_inst_alu_op      (_iq1_io_insts_issue_3_inst_alu_op),
    .io_insts_issue_3_inst_alu_rs1_sel (_iq1_io_insts_issue_3_inst_alu_rs1_sel),
    .io_insts_issue_3_inst_alu_rs2_sel (_iq1_io_insts_issue_3_inst_alu_rs2_sel),
    .io_insts_issue_3_inst_pc          (_iq1_io_insts_issue_3_inst_pc),
    .io_insts_issue_4_inst_prj         (_iq1_io_insts_issue_4_inst_prj),
    .io_insts_issue_4_inst_prk         (_iq1_io_insts_issue_4_inst_prk),
    .io_insts_issue_4_inst_rd_valid    (_iq1_io_insts_issue_4_inst_rd_valid),
    .io_insts_issue_4_inst_prd         (_iq1_io_insts_issue_4_inst_prd),
    .io_insts_issue_4_inst_imm         (_iq1_io_insts_issue_4_inst_imm),
    .io_insts_issue_4_inst_rob_index   (_iq1_io_insts_issue_4_inst_rob_index),
    .io_insts_issue_4_inst_alu_op      (_iq1_io_insts_issue_4_inst_alu_op),
    .io_insts_issue_4_inst_alu_rs1_sel (_iq1_io_insts_issue_4_inst_alu_rs1_sel),
    .io_insts_issue_4_inst_alu_rs2_sel (_iq1_io_insts_issue_4_inst_alu_rs2_sel),
    .io_insts_issue_4_inst_pc          (_iq1_io_insts_issue_4_inst_pc),
    .io_insts_issue_5_inst_prj         (_iq1_io_insts_issue_5_inst_prj),
    .io_insts_issue_5_inst_prk         (_iq1_io_insts_issue_5_inst_prk),
    .io_insts_issue_5_inst_rd_valid    (_iq1_io_insts_issue_5_inst_rd_valid),
    .io_insts_issue_5_inst_prd         (_iq1_io_insts_issue_5_inst_prd),
    .io_insts_issue_5_inst_imm         (_iq1_io_insts_issue_5_inst_imm),
    .io_insts_issue_5_inst_rob_index   (_iq1_io_insts_issue_5_inst_rob_index),
    .io_insts_issue_5_inst_alu_op      (_iq1_io_insts_issue_5_inst_alu_op),
    .io_insts_issue_5_inst_alu_rs1_sel (_iq1_io_insts_issue_5_inst_alu_rs1_sel),
    .io_insts_issue_5_inst_alu_rs2_sel (_iq1_io_insts_issue_5_inst_alu_rs2_sel),
    .io_insts_issue_5_inst_pc          (_iq1_io_insts_issue_5_inst_pc),
    .io_issue_req_0                    (_iq1_io_issue_req_0),
    .io_issue_req_1                    (_iq1_io_issue_req_1),
    .io_issue_req_2                    (_iq1_io_issue_req_2),
    .io_issue_req_3                    (_iq1_io_issue_req_3),
    .io_issue_req_4                    (_iq1_io_issue_req_4),
    .io_issue_req_5                    (_iq1_io_issue_req_5),
    .io_stall                          (_sel1_io_stall),
    .io_issue_ack_0                    (_iq1_io_issue_ack_0),
    .io_issue_ack_1                    (_iq1_io_issue_ack_1),
    .io_issue_ack_2                    (_iq1_io_issue_ack_2),
    .io_issue_ack_3                    (_iq1_io_issue_ack_3),
    .io_issue_ack_4                    (_iq1_io_issue_ack_4),
    .io_issue_ack_5                    (_iq1_io_issue_ack_5),
    .io_wake_preg                      (_sel1_io_wake_preg),
    .io_inst_issue_inst_prj            (_sel1_io_inst_issue_inst_prj),
    .io_inst_issue_inst_prk            (_sel1_io_inst_issue_inst_prk),
    .io_inst_issue_inst_rd_valid       (_sel1_io_inst_issue_inst_rd_valid),
    .io_inst_issue_inst_prd            (_sel1_io_inst_issue_inst_prd),
    .io_inst_issue_inst_imm            (_sel1_io_inst_issue_inst_imm),
    .io_inst_issue_inst_rob_index      (_sel1_io_inst_issue_inst_rob_index),
    .io_inst_issue_inst_alu_op         (_sel1_io_inst_issue_inst_alu_op),
    .io_inst_issue_inst_alu_rs1_sel    (_sel1_io_inst_issue_inst_alu_rs1_sel),
    .io_inst_issue_inst_alu_rs2_sel    (_sel1_io_inst_issue_inst_alu_rs2_sel),
    .io_inst_issue_inst_pc             (_sel1_io_inst_issue_inst_pc),
    .io_inst_issue_valid               (_sel1_io_inst_issue_valid)
  );
  assign _ir_reg1_io_flush = _rob_io_predict_fail_cmt[7];
  IS_RF_Reg ir_reg1 (
    .clock                       (clock),
    .reset                       (reset),
    .io_flush                    (_ir_reg1_io_flush),
    .io_inst_pack_IS_prj         (_sel1_io_inst_issue_inst_prj),
    .io_inst_pack_IS_prk         (_sel1_io_inst_issue_inst_prk),
    .io_inst_pack_IS_rd_valid    (_sel1_io_inst_issue_inst_rd_valid),
    .io_inst_pack_IS_prd         (_sel1_io_inst_issue_inst_prd),
    .io_inst_pack_IS_imm         (_sel1_io_inst_issue_inst_imm),
    .io_inst_pack_IS_rob_index   (_sel1_io_inst_issue_inst_rob_index),
    .io_inst_pack_IS_alu_op      (_sel1_io_inst_issue_inst_alu_op),
    .io_inst_pack_IS_alu_rs1_sel (_sel1_io_inst_issue_inst_alu_rs1_sel),
    .io_inst_pack_IS_alu_rs2_sel (_sel1_io_inst_issue_inst_alu_rs2_sel),
    .io_inst_pack_IS_pc          (_sel1_io_inst_issue_inst_pc),
    .io_inst_pack_IS_inst_valid  (_sel1_io_inst_issue_valid),
    .io_inst_pack_RF_prj         (_ir_reg1_io_inst_pack_RF_prj),
    .io_inst_pack_RF_prk         (_ir_reg1_io_inst_pack_RF_prk),
    .io_inst_pack_RF_rd_valid    (_ir_reg1_io_inst_pack_RF_rd_valid),
    .io_inst_pack_RF_prd         (_ir_reg1_io_inst_pack_RF_prd),
    .io_inst_pack_RF_imm         (_ir_reg1_io_inst_pack_RF_imm),
    .io_inst_pack_RF_rob_index   (_ir_reg1_io_inst_pack_RF_rob_index),
    .io_inst_pack_RF_alu_op      (_ir_reg1_io_inst_pack_RF_alu_op),
    .io_inst_pack_RF_alu_rs1_sel (_ir_reg1_io_inst_pack_RF_alu_rs1_sel),
    .io_inst_pack_RF_alu_rs2_sel (_ir_reg1_io_inst_pack_RF_alu_rs2_sel),
    .io_inst_pack_RF_pc          (_ir_reg1_io_inst_pack_RF_pc),
    .io_inst_pack_RF_inst_valid  (_ir_reg1_io_inst_pack_RF_inst_valid)
  );
  assign _iq2_io_insts_dispatch_0_rob_index = _rob_io_rob_index_dp_0;
  assign _iq2_io_insts_dispatch_1_rob_index = _rob_io_rob_index_dp_1;
  assign _iq2_io_wake_preg_1 = _sel2_io_wake_preg;
  assign _iq2_io_ld_mem_prd = _ls_ex_mem_reg_io_prd_MEM_1[5:0];
  assign _iq2_io_flush = _rob_io_predict_fail_cmt[6];
  Unorder_Issue_Queue_1 iq2 (
    .clock                              (clock),
    .reset                              (reset),
    .io_insts_disp_valid_0              (_dp_io_insts_disp_valid_1_0),
    .io_insts_disp_valid_1              (_dp_io_insts_disp_valid_1_1),
    .io_insts_dispatch_0_prj            (_rename_io_prj_0),
    .io_insts_dispatch_0_prk            (_rename_io_prk_0),
    .io_insts_dispatch_0_rd_valid       (_dr_reg_io_insts_pack_RN_0_rd_valid),
    .io_insts_dispatch_0_prd            (inst_pack_RN_prd),
    .io_insts_dispatch_0_imm            (_dr_reg_io_insts_pack_RN_0_imm),
    .io_insts_dispatch_0_rob_index      (_iq2_io_insts_dispatch_0_rob_index),
    .io_insts_dispatch_0_alu_op         (_dr_reg_io_insts_pack_RN_0_alu_op),
    .io_insts_dispatch_0_alu_rs1_sel    (_dr_reg_io_insts_pack_RN_0_alu_rs1_sel),
    .io_insts_dispatch_0_alu_rs2_sel    (_dr_reg_io_insts_pack_RN_0_alu_rs2_sel),
    .io_insts_dispatch_0_pc             (_dr_reg_io_insts_pack_RN_0_pc),
    .io_insts_dispatch_0_br_type        (_dr_reg_io_insts_pack_RN_0_br_type),
    .io_insts_dispatch_0_predict_jump   (_dr_reg_io_insts_pack_RN_0_predict_jump),
    .io_insts_dispatch_0_pred_npc       (_dr_reg_io_insts_pack_RN_0_pred_npc),
    .io_insts_dispatch_1_prj            (_rename_io_prj_1),
    .io_insts_dispatch_1_prk            (_rename_io_prk_1),
    .io_insts_dispatch_1_rd_valid       (_dr_reg_io_insts_pack_RN_1_rd_valid),
    .io_insts_dispatch_1_prd            (inst_pack_RN_1_prd),
    .io_insts_dispatch_1_imm            (_dr_reg_io_insts_pack_RN_1_imm),
    .io_insts_dispatch_1_rob_index      (_iq2_io_insts_dispatch_1_rob_index),
    .io_insts_dispatch_1_alu_op         (_dr_reg_io_insts_pack_RN_1_alu_op),
    .io_insts_dispatch_1_alu_rs1_sel    (_dr_reg_io_insts_pack_RN_1_alu_rs1_sel),
    .io_insts_dispatch_1_alu_rs2_sel    (_dr_reg_io_insts_pack_RN_1_alu_rs2_sel),
    .io_insts_dispatch_1_pc             (_dr_reg_io_insts_pack_RN_1_pc),
    .io_insts_dispatch_1_br_type        (_dr_reg_io_insts_pack_RN_1_br_type),
    .io_insts_dispatch_1_predict_jump   (_dr_reg_io_insts_pack_RN_1_predict_jump),
    .io_insts_dispatch_1_pred_npc       (_dr_reg_io_insts_pack_RN_1_pred_npc),
    .io_prj_ready_0                     (_rename_io_prj_ready_0),
    .io_prj_ready_1                     (_rename_io_prj_ready_1),
    .io_prk_ready_0                     (_rename_io_prk_ready_0),
    .io_prk_ready_1                     (_rename_io_prk_ready_1),
    .io_wake_preg_0                     (_sel1_io_wake_preg),
    .io_wake_preg_1                     (_iq2_io_wake_preg_1),
    .io_wake_preg_2                     (iq_mutual_wake_preg_2),
    .io_wake_preg_3                     (iq_mutual_wake_preg_3),
    .io_ld_mem_prd                      (_iq2_io_ld_mem_prd),
    .io_issue_ack_0                     (_iq2_io_issue_ack_0),
    .io_issue_ack_1                     (_iq2_io_issue_ack_1),
    .io_issue_ack_2                     (_iq2_io_issue_ack_2),
    .io_issue_ack_3                     (_iq2_io_issue_ack_3),
    .io_issue_ack_4                     (_iq2_io_issue_ack_4),
    .io_issue_ack_5                     (_iq2_io_issue_ack_5),
    .io_insts_issue_0_inst_prj          (_iq2_io_insts_issue_0_inst_prj),
    .io_insts_issue_0_inst_prk          (_iq2_io_insts_issue_0_inst_prk),
    .io_insts_issue_0_inst_rd_valid     (_iq2_io_insts_issue_0_inst_rd_valid),
    .io_insts_issue_0_inst_prd          (_iq2_io_insts_issue_0_inst_prd),
    .io_insts_issue_0_inst_imm          (_iq2_io_insts_issue_0_inst_imm),
    .io_insts_issue_0_inst_rob_index    (_iq2_io_insts_issue_0_inst_rob_index),
    .io_insts_issue_0_inst_alu_op       (_iq2_io_insts_issue_0_inst_alu_op),
    .io_insts_issue_0_inst_alu_rs1_sel  (_iq2_io_insts_issue_0_inst_alu_rs1_sel),
    .io_insts_issue_0_inst_alu_rs2_sel  (_iq2_io_insts_issue_0_inst_alu_rs2_sel),
    .io_insts_issue_0_inst_pc           (_iq2_io_insts_issue_0_inst_pc),
    .io_insts_issue_0_inst_br_type      (_iq2_io_insts_issue_0_inst_br_type),
    .io_insts_issue_0_inst_predict_jump (_iq2_io_insts_issue_0_inst_predict_jump),
    .io_insts_issue_0_inst_pred_npc     (_iq2_io_insts_issue_0_inst_pred_npc),
    .io_insts_issue_1_inst_prj          (_iq2_io_insts_issue_1_inst_prj),
    .io_insts_issue_1_inst_prk          (_iq2_io_insts_issue_1_inst_prk),
    .io_insts_issue_1_inst_rd_valid     (_iq2_io_insts_issue_1_inst_rd_valid),
    .io_insts_issue_1_inst_prd          (_iq2_io_insts_issue_1_inst_prd),
    .io_insts_issue_1_inst_imm          (_iq2_io_insts_issue_1_inst_imm),
    .io_insts_issue_1_inst_rob_index    (_iq2_io_insts_issue_1_inst_rob_index),
    .io_insts_issue_1_inst_alu_op       (_iq2_io_insts_issue_1_inst_alu_op),
    .io_insts_issue_1_inst_alu_rs1_sel  (_iq2_io_insts_issue_1_inst_alu_rs1_sel),
    .io_insts_issue_1_inst_alu_rs2_sel  (_iq2_io_insts_issue_1_inst_alu_rs2_sel),
    .io_insts_issue_1_inst_pc           (_iq2_io_insts_issue_1_inst_pc),
    .io_insts_issue_1_inst_br_type      (_iq2_io_insts_issue_1_inst_br_type),
    .io_insts_issue_1_inst_predict_jump (_iq2_io_insts_issue_1_inst_predict_jump),
    .io_insts_issue_1_inst_pred_npc     (_iq2_io_insts_issue_1_inst_pred_npc),
    .io_insts_issue_2_inst_prj          (_iq2_io_insts_issue_2_inst_prj),
    .io_insts_issue_2_inst_prk          (_iq2_io_insts_issue_2_inst_prk),
    .io_insts_issue_2_inst_rd_valid     (_iq2_io_insts_issue_2_inst_rd_valid),
    .io_insts_issue_2_inst_prd          (_iq2_io_insts_issue_2_inst_prd),
    .io_insts_issue_2_inst_imm          (_iq2_io_insts_issue_2_inst_imm),
    .io_insts_issue_2_inst_rob_index    (_iq2_io_insts_issue_2_inst_rob_index),
    .io_insts_issue_2_inst_alu_op       (_iq2_io_insts_issue_2_inst_alu_op),
    .io_insts_issue_2_inst_alu_rs1_sel  (_iq2_io_insts_issue_2_inst_alu_rs1_sel),
    .io_insts_issue_2_inst_alu_rs2_sel  (_iq2_io_insts_issue_2_inst_alu_rs2_sel),
    .io_insts_issue_2_inst_pc           (_iq2_io_insts_issue_2_inst_pc),
    .io_insts_issue_2_inst_br_type      (_iq2_io_insts_issue_2_inst_br_type),
    .io_insts_issue_2_inst_predict_jump (_iq2_io_insts_issue_2_inst_predict_jump),
    .io_insts_issue_2_inst_pred_npc     (_iq2_io_insts_issue_2_inst_pred_npc),
    .io_insts_issue_3_inst_prj          (_iq2_io_insts_issue_3_inst_prj),
    .io_insts_issue_3_inst_prk          (_iq2_io_insts_issue_3_inst_prk),
    .io_insts_issue_3_inst_rd_valid     (_iq2_io_insts_issue_3_inst_rd_valid),
    .io_insts_issue_3_inst_prd          (_iq2_io_insts_issue_3_inst_prd),
    .io_insts_issue_3_inst_imm          (_iq2_io_insts_issue_3_inst_imm),
    .io_insts_issue_3_inst_rob_index    (_iq2_io_insts_issue_3_inst_rob_index),
    .io_insts_issue_3_inst_alu_op       (_iq2_io_insts_issue_3_inst_alu_op),
    .io_insts_issue_3_inst_alu_rs1_sel  (_iq2_io_insts_issue_3_inst_alu_rs1_sel),
    .io_insts_issue_3_inst_alu_rs2_sel  (_iq2_io_insts_issue_3_inst_alu_rs2_sel),
    .io_insts_issue_3_inst_pc           (_iq2_io_insts_issue_3_inst_pc),
    .io_insts_issue_3_inst_br_type      (_iq2_io_insts_issue_3_inst_br_type),
    .io_insts_issue_3_inst_predict_jump (_iq2_io_insts_issue_3_inst_predict_jump),
    .io_insts_issue_3_inst_pred_npc     (_iq2_io_insts_issue_3_inst_pred_npc),
    .io_insts_issue_4_inst_prj          (_iq2_io_insts_issue_4_inst_prj),
    .io_insts_issue_4_inst_prk          (_iq2_io_insts_issue_4_inst_prk),
    .io_insts_issue_4_inst_rd_valid     (_iq2_io_insts_issue_4_inst_rd_valid),
    .io_insts_issue_4_inst_prd          (_iq2_io_insts_issue_4_inst_prd),
    .io_insts_issue_4_inst_imm          (_iq2_io_insts_issue_4_inst_imm),
    .io_insts_issue_4_inst_rob_index    (_iq2_io_insts_issue_4_inst_rob_index),
    .io_insts_issue_4_inst_alu_op       (_iq2_io_insts_issue_4_inst_alu_op),
    .io_insts_issue_4_inst_alu_rs1_sel  (_iq2_io_insts_issue_4_inst_alu_rs1_sel),
    .io_insts_issue_4_inst_alu_rs2_sel  (_iq2_io_insts_issue_4_inst_alu_rs2_sel),
    .io_insts_issue_4_inst_pc           (_iq2_io_insts_issue_4_inst_pc),
    .io_insts_issue_4_inst_br_type      (_iq2_io_insts_issue_4_inst_br_type),
    .io_insts_issue_4_inst_predict_jump (_iq2_io_insts_issue_4_inst_predict_jump),
    .io_insts_issue_4_inst_pred_npc     (_iq2_io_insts_issue_4_inst_pred_npc),
    .io_insts_issue_5_inst_prj          (_iq2_io_insts_issue_5_inst_prj),
    .io_insts_issue_5_inst_prk          (_iq2_io_insts_issue_5_inst_prk),
    .io_insts_issue_5_inst_rd_valid     (_iq2_io_insts_issue_5_inst_rd_valid),
    .io_insts_issue_5_inst_prd          (_iq2_io_insts_issue_5_inst_prd),
    .io_insts_issue_5_inst_imm          (_iq2_io_insts_issue_5_inst_imm),
    .io_insts_issue_5_inst_rob_index    (_iq2_io_insts_issue_5_inst_rob_index),
    .io_insts_issue_5_inst_alu_op       (_iq2_io_insts_issue_5_inst_alu_op),
    .io_insts_issue_5_inst_alu_rs1_sel  (_iq2_io_insts_issue_5_inst_alu_rs1_sel),
    .io_insts_issue_5_inst_alu_rs2_sel  (_iq2_io_insts_issue_5_inst_alu_rs2_sel),
    .io_insts_issue_5_inst_pc           (_iq2_io_insts_issue_5_inst_pc),
    .io_insts_issue_5_inst_br_type      (_iq2_io_insts_issue_5_inst_br_type),
    .io_insts_issue_5_inst_predict_jump (_iq2_io_insts_issue_5_inst_predict_jump),
    .io_insts_issue_5_inst_pred_npc     (_iq2_io_insts_issue_5_inst_pred_npc),
    .io_issue_req_0                     (_iq2_io_issue_req_0),
    .io_issue_req_1                     (_iq2_io_issue_req_1),
    .io_issue_req_2                     (_iq2_io_issue_req_2),
    .io_issue_req_3                     (_iq2_io_issue_req_3),
    .io_issue_req_4                     (_iq2_io_issue_req_4),
    .io_issue_req_5                     (_iq2_io_issue_req_5),
    .io_elem_num                        (_dp_io_elem_num_1),
    .io_full                            (_iq2_io_full),
    .io_stall                           (_iq4_io_stall_T),
    .io_flush                           (_iq2_io_flush),
    .io_dcache_miss                     (_iq2_io_dcache_miss)
  );
  assign _sel2_io_stall =
    {_iq2_io_issue_req_5,
     _iq2_io_issue_req_4,
     _iq2_io_issue_req_3,
     _iq2_io_issue_req_2,
     _iq2_io_issue_req_1,
     _iq2_io_issue_req_0} == 6'h0;
  Unorder_Select_1 sel2 (
    .io_insts_issue_0_inst_prj          (_iq2_io_insts_issue_0_inst_prj),
    .io_insts_issue_0_inst_prk          (_iq2_io_insts_issue_0_inst_prk),
    .io_insts_issue_0_inst_rd_valid     (_iq2_io_insts_issue_0_inst_rd_valid),
    .io_insts_issue_0_inst_prd          (_iq2_io_insts_issue_0_inst_prd),
    .io_insts_issue_0_inst_imm          (_iq2_io_insts_issue_0_inst_imm),
    .io_insts_issue_0_inst_rob_index    (_iq2_io_insts_issue_0_inst_rob_index),
    .io_insts_issue_0_inst_alu_op       (_iq2_io_insts_issue_0_inst_alu_op),
    .io_insts_issue_0_inst_alu_rs1_sel  (_iq2_io_insts_issue_0_inst_alu_rs1_sel),
    .io_insts_issue_0_inst_alu_rs2_sel  (_iq2_io_insts_issue_0_inst_alu_rs2_sel),
    .io_insts_issue_0_inst_pc           (_iq2_io_insts_issue_0_inst_pc),
    .io_insts_issue_0_inst_br_type      (_iq2_io_insts_issue_0_inst_br_type),
    .io_insts_issue_0_inst_predict_jump (_iq2_io_insts_issue_0_inst_predict_jump),
    .io_insts_issue_0_inst_pred_npc     (_iq2_io_insts_issue_0_inst_pred_npc),
    .io_insts_issue_1_inst_prj          (_iq2_io_insts_issue_1_inst_prj),
    .io_insts_issue_1_inst_prk          (_iq2_io_insts_issue_1_inst_prk),
    .io_insts_issue_1_inst_rd_valid     (_iq2_io_insts_issue_1_inst_rd_valid),
    .io_insts_issue_1_inst_prd          (_iq2_io_insts_issue_1_inst_prd),
    .io_insts_issue_1_inst_imm          (_iq2_io_insts_issue_1_inst_imm),
    .io_insts_issue_1_inst_rob_index    (_iq2_io_insts_issue_1_inst_rob_index),
    .io_insts_issue_1_inst_alu_op       (_iq2_io_insts_issue_1_inst_alu_op),
    .io_insts_issue_1_inst_alu_rs1_sel  (_iq2_io_insts_issue_1_inst_alu_rs1_sel),
    .io_insts_issue_1_inst_alu_rs2_sel  (_iq2_io_insts_issue_1_inst_alu_rs2_sel),
    .io_insts_issue_1_inst_pc           (_iq2_io_insts_issue_1_inst_pc),
    .io_insts_issue_1_inst_br_type      (_iq2_io_insts_issue_1_inst_br_type),
    .io_insts_issue_1_inst_predict_jump (_iq2_io_insts_issue_1_inst_predict_jump),
    .io_insts_issue_1_inst_pred_npc     (_iq2_io_insts_issue_1_inst_pred_npc),
    .io_insts_issue_2_inst_prj          (_iq2_io_insts_issue_2_inst_prj),
    .io_insts_issue_2_inst_prk          (_iq2_io_insts_issue_2_inst_prk),
    .io_insts_issue_2_inst_rd_valid     (_iq2_io_insts_issue_2_inst_rd_valid),
    .io_insts_issue_2_inst_prd          (_iq2_io_insts_issue_2_inst_prd),
    .io_insts_issue_2_inst_imm          (_iq2_io_insts_issue_2_inst_imm),
    .io_insts_issue_2_inst_rob_index    (_iq2_io_insts_issue_2_inst_rob_index),
    .io_insts_issue_2_inst_alu_op       (_iq2_io_insts_issue_2_inst_alu_op),
    .io_insts_issue_2_inst_alu_rs1_sel  (_iq2_io_insts_issue_2_inst_alu_rs1_sel),
    .io_insts_issue_2_inst_alu_rs2_sel  (_iq2_io_insts_issue_2_inst_alu_rs2_sel),
    .io_insts_issue_2_inst_pc           (_iq2_io_insts_issue_2_inst_pc),
    .io_insts_issue_2_inst_br_type      (_iq2_io_insts_issue_2_inst_br_type),
    .io_insts_issue_2_inst_predict_jump (_iq2_io_insts_issue_2_inst_predict_jump),
    .io_insts_issue_2_inst_pred_npc     (_iq2_io_insts_issue_2_inst_pred_npc),
    .io_insts_issue_3_inst_prj          (_iq2_io_insts_issue_3_inst_prj),
    .io_insts_issue_3_inst_prk          (_iq2_io_insts_issue_3_inst_prk),
    .io_insts_issue_3_inst_rd_valid     (_iq2_io_insts_issue_3_inst_rd_valid),
    .io_insts_issue_3_inst_prd          (_iq2_io_insts_issue_3_inst_prd),
    .io_insts_issue_3_inst_imm          (_iq2_io_insts_issue_3_inst_imm),
    .io_insts_issue_3_inst_rob_index    (_iq2_io_insts_issue_3_inst_rob_index),
    .io_insts_issue_3_inst_alu_op       (_iq2_io_insts_issue_3_inst_alu_op),
    .io_insts_issue_3_inst_alu_rs1_sel  (_iq2_io_insts_issue_3_inst_alu_rs1_sel),
    .io_insts_issue_3_inst_alu_rs2_sel  (_iq2_io_insts_issue_3_inst_alu_rs2_sel),
    .io_insts_issue_3_inst_pc           (_iq2_io_insts_issue_3_inst_pc),
    .io_insts_issue_3_inst_br_type      (_iq2_io_insts_issue_3_inst_br_type),
    .io_insts_issue_3_inst_predict_jump (_iq2_io_insts_issue_3_inst_predict_jump),
    .io_insts_issue_3_inst_pred_npc     (_iq2_io_insts_issue_3_inst_pred_npc),
    .io_insts_issue_4_inst_prj          (_iq2_io_insts_issue_4_inst_prj),
    .io_insts_issue_4_inst_prk          (_iq2_io_insts_issue_4_inst_prk),
    .io_insts_issue_4_inst_rd_valid     (_iq2_io_insts_issue_4_inst_rd_valid),
    .io_insts_issue_4_inst_prd          (_iq2_io_insts_issue_4_inst_prd),
    .io_insts_issue_4_inst_imm          (_iq2_io_insts_issue_4_inst_imm),
    .io_insts_issue_4_inst_rob_index    (_iq2_io_insts_issue_4_inst_rob_index),
    .io_insts_issue_4_inst_alu_op       (_iq2_io_insts_issue_4_inst_alu_op),
    .io_insts_issue_4_inst_alu_rs1_sel  (_iq2_io_insts_issue_4_inst_alu_rs1_sel),
    .io_insts_issue_4_inst_alu_rs2_sel  (_iq2_io_insts_issue_4_inst_alu_rs2_sel),
    .io_insts_issue_4_inst_pc           (_iq2_io_insts_issue_4_inst_pc),
    .io_insts_issue_4_inst_br_type      (_iq2_io_insts_issue_4_inst_br_type),
    .io_insts_issue_4_inst_predict_jump (_iq2_io_insts_issue_4_inst_predict_jump),
    .io_insts_issue_4_inst_pred_npc     (_iq2_io_insts_issue_4_inst_pred_npc),
    .io_insts_issue_5_inst_prj          (_iq2_io_insts_issue_5_inst_prj),
    .io_insts_issue_5_inst_prk          (_iq2_io_insts_issue_5_inst_prk),
    .io_insts_issue_5_inst_rd_valid     (_iq2_io_insts_issue_5_inst_rd_valid),
    .io_insts_issue_5_inst_prd          (_iq2_io_insts_issue_5_inst_prd),
    .io_insts_issue_5_inst_imm          (_iq2_io_insts_issue_5_inst_imm),
    .io_insts_issue_5_inst_rob_index    (_iq2_io_insts_issue_5_inst_rob_index),
    .io_insts_issue_5_inst_alu_op       (_iq2_io_insts_issue_5_inst_alu_op),
    .io_insts_issue_5_inst_alu_rs1_sel  (_iq2_io_insts_issue_5_inst_alu_rs1_sel),
    .io_insts_issue_5_inst_alu_rs2_sel  (_iq2_io_insts_issue_5_inst_alu_rs2_sel),
    .io_insts_issue_5_inst_pc           (_iq2_io_insts_issue_5_inst_pc),
    .io_insts_issue_5_inst_br_type      (_iq2_io_insts_issue_5_inst_br_type),
    .io_insts_issue_5_inst_predict_jump (_iq2_io_insts_issue_5_inst_predict_jump),
    .io_insts_issue_5_inst_pred_npc     (_iq2_io_insts_issue_5_inst_pred_npc),
    .io_issue_req_0                     (_iq2_io_issue_req_0),
    .io_issue_req_1                     (_iq2_io_issue_req_1),
    .io_issue_req_2                     (_iq2_io_issue_req_2),
    .io_issue_req_3                     (_iq2_io_issue_req_3),
    .io_issue_req_4                     (_iq2_io_issue_req_4),
    .io_issue_req_5                     (_iq2_io_issue_req_5),
    .io_stall                           (_sel2_io_stall),
    .io_issue_ack_0                     (_iq2_io_issue_ack_0),
    .io_issue_ack_1                     (_iq2_io_issue_ack_1),
    .io_issue_ack_2                     (_iq2_io_issue_ack_2),
    .io_issue_ack_3                     (_iq2_io_issue_ack_3),
    .io_issue_ack_4                     (_iq2_io_issue_ack_4),
    .io_issue_ack_5                     (_iq2_io_issue_ack_5),
    .io_wake_preg                       (_sel2_io_wake_preg),
    .io_inst_issue_inst_prj             (_sel2_io_inst_issue_inst_prj),
    .io_inst_issue_inst_prk             (_sel2_io_inst_issue_inst_prk),
    .io_inst_issue_inst_rd_valid        (_sel2_io_inst_issue_inst_rd_valid),
    .io_inst_issue_inst_prd             (_sel2_io_inst_issue_inst_prd),
    .io_inst_issue_inst_imm             (_sel2_io_inst_issue_inst_imm),
    .io_inst_issue_inst_rob_index       (_sel2_io_inst_issue_inst_rob_index),
    .io_inst_issue_inst_alu_op          (_sel2_io_inst_issue_inst_alu_op),
    .io_inst_issue_inst_alu_rs1_sel     (_sel2_io_inst_issue_inst_alu_rs1_sel),
    .io_inst_issue_inst_alu_rs2_sel     (_sel2_io_inst_issue_inst_alu_rs2_sel),
    .io_inst_issue_inst_pc              (_sel2_io_inst_issue_inst_pc),
    .io_inst_issue_inst_br_type         (_sel2_io_inst_issue_inst_br_type),
    .io_inst_issue_inst_predict_jump    (_sel2_io_inst_issue_inst_predict_jump),
    .io_inst_issue_inst_pred_npc        (_sel2_io_inst_issue_inst_pred_npc),
    .io_inst_issue_valid                (_sel2_io_inst_issue_valid)
  );
  assign _ir_reg2_io_flush = _rob_io_predict_fail_cmt[7];
  IS_RF_Reg_1 ir_reg2 (
    .clock                        (clock),
    .reset                        (reset),
    .io_flush                     (_ir_reg2_io_flush),
    .io_inst_pack_IS_prj          (_sel2_io_inst_issue_inst_prj),
    .io_inst_pack_IS_prk          (_sel2_io_inst_issue_inst_prk),
    .io_inst_pack_IS_rd_valid     (_sel2_io_inst_issue_inst_rd_valid),
    .io_inst_pack_IS_prd          (_sel2_io_inst_issue_inst_prd),
    .io_inst_pack_IS_imm          (_sel2_io_inst_issue_inst_imm),
    .io_inst_pack_IS_rob_index    (_sel2_io_inst_issue_inst_rob_index),
    .io_inst_pack_IS_alu_op       (_sel2_io_inst_issue_inst_alu_op),
    .io_inst_pack_IS_alu_rs1_sel  (_sel2_io_inst_issue_inst_alu_rs1_sel),
    .io_inst_pack_IS_alu_rs2_sel  (_sel2_io_inst_issue_inst_alu_rs2_sel),
    .io_inst_pack_IS_pc           (_sel2_io_inst_issue_inst_pc),
    .io_inst_pack_IS_br_type      (_sel2_io_inst_issue_inst_br_type),
    .io_inst_pack_IS_predict_jump (_sel2_io_inst_issue_inst_predict_jump),
    .io_inst_pack_IS_pred_npc     (_sel2_io_inst_issue_inst_pred_npc),
    .io_inst_pack_IS_inst_valid   (_sel2_io_inst_issue_valid),
    .io_inst_pack_RF_prj          (_ir_reg2_io_inst_pack_RF_prj),
    .io_inst_pack_RF_prk          (_ir_reg2_io_inst_pack_RF_prk),
    .io_inst_pack_RF_rd_valid     (_ir_reg2_io_inst_pack_RF_rd_valid),
    .io_inst_pack_RF_prd          (_ir_reg2_io_inst_pack_RF_prd),
    .io_inst_pack_RF_imm          (_ir_reg2_io_inst_pack_RF_imm),
    .io_inst_pack_RF_rob_index    (_ir_reg2_io_inst_pack_RF_rob_index),
    .io_inst_pack_RF_alu_op       (_ir_reg2_io_inst_pack_RF_alu_op),
    .io_inst_pack_RF_alu_rs1_sel  (_ir_reg2_io_inst_pack_RF_alu_rs1_sel),
    .io_inst_pack_RF_alu_rs2_sel  (_ir_reg2_io_inst_pack_RF_alu_rs2_sel),
    .io_inst_pack_RF_pc           (_ir_reg2_io_inst_pack_RF_pc),
    .io_inst_pack_RF_br_type      (_ir_reg2_io_inst_pack_RF_br_type),
    .io_inst_pack_RF_predict_jump (_ir_reg2_io_inst_pack_RF_predict_jump),
    .io_inst_pack_RF_pred_npc     (_ir_reg2_io_inst_pack_RF_pred_npc),
    .io_inst_pack_RF_inst_valid   (_ir_reg2_io_inst_pack_RF_inst_valid)
  );
  assign _iq3_io_insts_dispatch_0_rob_index = _rob_io_rob_index_dp_0;
  assign _iq3_io_insts_dispatch_0_priv_vec = _dr_reg_io_insts_pack_RN_0_priv_vec[9:0];
  assign _iq3_io_insts_dispatch_1_rob_index = _rob_io_rob_index_dp_1;
  assign _iq3_io_insts_dispatch_1_priv_vec = _dr_reg_io_insts_pack_RN_1_priv_vec[9:0];
  assign _iq3_io_ld_mem_prd = _ls_ex_mem_reg_io_prd_MEM_2[5:0];
  assign _iq3_io_flush = _rob_io_predict_fail_cmt[6];
  Order_Issue_Queue iq3 (
    .clock                         (clock),
    .reset                         (reset),
    .io_insts_disp_valid_0         (_dp_io_insts_disp_valid_2_0),
    .io_insts_disp_valid_1         (_dp_io_insts_disp_valid_2_1),
    .io_insts_dispatch_0_prj       (_rename_io_prj_0),
    .io_insts_dispatch_0_prk       (_rename_io_prk_0),
    .io_insts_dispatch_0_rd_valid  (_dr_reg_io_insts_pack_RN_0_rd_valid),
    .io_insts_dispatch_0_prd       (inst_pack_RN_prd),
    .io_insts_dispatch_0_imm       (_dr_reg_io_insts_pack_RN_0_imm),
    .io_insts_dispatch_0_rob_index (_iq3_io_insts_dispatch_0_rob_index),
    .io_insts_dispatch_0_priv_vec  (_iq3_io_insts_dispatch_0_priv_vec),
    .io_insts_dispatch_0_alu_op    (_dr_reg_io_insts_pack_RN_0_alu_op),
    .io_insts_dispatch_1_prj       (_rename_io_prj_1),
    .io_insts_dispatch_1_prk       (_rename_io_prk_1),
    .io_insts_dispatch_1_rd_valid  (_dr_reg_io_insts_pack_RN_1_rd_valid),
    .io_insts_dispatch_1_prd       (inst_pack_RN_1_prd),
    .io_insts_dispatch_1_imm       (_dr_reg_io_insts_pack_RN_1_imm),
    .io_insts_dispatch_1_rob_index (_iq3_io_insts_dispatch_1_rob_index),
    .io_insts_dispatch_1_priv_vec  (_iq3_io_insts_dispatch_1_priv_vec),
    .io_insts_dispatch_1_alu_op    (_dr_reg_io_insts_pack_RN_1_alu_op),
    .io_prj_ready_0                (_rename_io_prj_ready_0),
    .io_prj_ready_1                (_rename_io_prj_ready_1),
    .io_prk_ready_0                (_rename_io_prk_ready_0),
    .io_prk_ready_1                (_rename_io_prk_ready_1),
    .io_wake_preg_0                (_ir_reg1_io_inst_pack_RF_prd),
    .io_wake_preg_1                (_ir_reg2_io_inst_pack_RF_prd),
    .io_wake_preg_2                (iq_inline_wake_preg_2),
    .io_wake_preg_3                (iq_mutual_wake_preg_3),
    .io_ld_mem_prd                 (_iq3_io_ld_mem_prd),
    .io_issue_ack                  (_iq3_io_issue_ack),
    .io_insts_issue_inst_prj       (_iq3_io_insts_issue_inst_prj),
    .io_insts_issue_inst_prk       (_iq3_io_insts_issue_inst_prk),
    .io_insts_issue_inst_rd_valid  (_iq3_io_insts_issue_inst_rd_valid),
    .io_insts_issue_inst_prd       (_iq3_io_insts_issue_inst_prd),
    .io_insts_issue_inst_imm       (_iq3_io_insts_issue_inst_imm),
    .io_insts_issue_inst_rob_index (_iq3_io_insts_issue_inst_rob_index),
    .io_insts_issue_inst_priv_vec  (_iq3_io_insts_issue_inst_priv_vec),
    .io_insts_issue_inst_alu_op    (_iq3_io_insts_issue_inst_alu_op),
    .io_issue_req                  (_iq3_io_issue_req),
    .io_full                       (_iq3_io_full),
    .io_stall                      (_iq4_io_stall_T),
    .io_flush                      (_iq3_io_flush),
    .io_dcache_miss                (_iq3_io_dcache_miss)
  );
  assign _sel3_io_stall = ~_iq3_io_issue_req | _mdu_io_busy_17 | sel3_io_stall_r;
  Order_Select sel3 (
    .io_insts_issue_inst_prj       (_iq3_io_insts_issue_inst_prj),
    .io_insts_issue_inst_prk       (_iq3_io_insts_issue_inst_prk),
    .io_insts_issue_inst_rd_valid  (_iq3_io_insts_issue_inst_rd_valid),
    .io_insts_issue_inst_prd       (_iq3_io_insts_issue_inst_prd),
    .io_insts_issue_inst_imm       (_iq3_io_insts_issue_inst_imm),
    .io_insts_issue_inst_rob_index (_iq3_io_insts_issue_inst_rob_index),
    .io_insts_issue_inst_priv_vec  (_iq3_io_insts_issue_inst_priv_vec),
    .io_insts_issue_inst_alu_op    (_iq3_io_insts_issue_inst_alu_op),
    .io_issue_req                  (_iq3_io_issue_req),
    .io_stall                      (_sel3_io_stall),
    .io_issue_ack                  (_iq3_io_issue_ack),
    .io_inst_issue_inst_prj        (_sel3_io_inst_issue_inst_prj),
    .io_inst_issue_inst_prk        (_sel3_io_inst_issue_inst_prk),
    .io_inst_issue_inst_rd_valid   (_sel3_io_inst_issue_inst_rd_valid),
    .io_inst_issue_inst_prd        (_sel3_io_inst_issue_inst_prd),
    .io_inst_issue_inst_imm        (_sel3_io_inst_issue_inst_imm),
    .io_inst_issue_inst_rob_index  (_sel3_io_inst_issue_inst_rob_index),
    .io_inst_issue_inst_priv_vec   (_sel3_io_inst_issue_inst_priv_vec),
    .io_inst_issue_inst_alu_op     (_sel3_io_inst_issue_inst_alu_op),
    .io_inst_issue_valid           (_sel3_io_inst_issue_valid)
  );
  assign _ir_reg3_io_flush = _rob_io_predict_fail_cmt[7];
  assign _ir_reg3_io_stall = _mdu_io_busy_17;
  IS_RF_Reg_2 ir_reg3 (
    .clock                      (clock),
    .reset                      (reset),
    .io_flush                   (_ir_reg3_io_flush),
    .io_stall                   (_ir_reg3_io_stall),
    .io_inst_pack_IS_prj        (_sel3_io_inst_issue_inst_prj),
    .io_inst_pack_IS_prk        (_sel3_io_inst_issue_inst_prk),
    .io_inst_pack_IS_rd_valid   (_sel3_io_inst_issue_inst_rd_valid),
    .io_inst_pack_IS_prd        (_sel3_io_inst_issue_inst_prd),
    .io_inst_pack_IS_imm        (_sel3_io_inst_issue_inst_imm),
    .io_inst_pack_IS_rob_index  (_sel3_io_inst_issue_inst_rob_index),
    .io_inst_pack_IS_priv_vec   (_sel3_io_inst_issue_inst_priv_vec),
    .io_inst_pack_IS_alu_op     (_sel3_io_inst_issue_inst_alu_op),
    .io_inst_pack_IS_inst_valid (_sel3_io_inst_issue_valid),
    .io_inst_pack_RF_prj        (_ir_reg3_io_inst_pack_RF_prj),
    .io_inst_pack_RF_prk        (_ir_reg3_io_inst_pack_RF_prk),
    .io_inst_pack_RF_rd_valid   (_ir_reg3_io_inst_pack_RF_rd_valid),
    .io_inst_pack_RF_prd        (_ir_reg3_io_inst_pack_RF_prd),
    .io_inst_pack_RF_imm        (_ir_reg3_io_inst_pack_RF_imm),
    .io_inst_pack_RF_rob_index  (_ir_reg3_io_inst_pack_RF_rob_index),
    .io_inst_pack_RF_priv_vec   (_ir_reg3_io_inst_pack_RF_priv_vec),
    .io_inst_pack_RF_alu_op     (_ir_reg3_io_inst_pack_RF_alu_op),
    .io_inst_pack_RF_inst_valid (_ir_reg3_io_inst_pack_RF_inst_valid)
  );
  assign _iq4_io_insts_dispatch_0_rob_index = _rob_io_rob_index_dp_0;
  assign _iq4_io_insts_dispatch_0_priv_vec = _dr_reg_io_insts_pack_RN_0_priv_vec[12:10];
  assign _iq4_io_insts_dispatch_1_rob_index = _rob_io_rob_index_dp_1;
  assign _iq4_io_insts_dispatch_1_priv_vec = _dr_reg_io_insts_pack_RN_1_priv_vec[12:10];
  assign _iq4_io_wake_preg_3 = _re_reg4_io_inst_pack_EX_prd;
  assign _iq4_io_is_store_cmt_num = _rob_io_is_store_num_cmt;
  assign _iq4_io_flush = _rob_io_predict_fail_cmt[6];
  Order_Issue_Queue_1 iq4 (
    .clock                         (clock),
    .reset                         (reset),
    .io_insts_disp_valid_0         (_dp_io_insts_disp_valid_3_0),
    .io_insts_disp_valid_1         (_dp_io_insts_disp_valid_3_1),
    .io_insts_dispatch_0_prj       (_rename_io_prj_0),
    .io_insts_dispatch_0_prk       (_rename_io_prk_0),
    .io_insts_dispatch_0_rd_valid  (_dr_reg_io_insts_pack_RN_0_rd_valid),
    .io_insts_dispatch_0_prd       (inst_pack_RN_prd),
    .io_insts_dispatch_0_imm       (_dr_reg_io_insts_pack_RN_0_imm),
    .io_insts_dispatch_0_rob_index (_iq4_io_insts_dispatch_0_rob_index),
    .io_insts_dispatch_0_mem_type  (_dr_reg_io_insts_pack_RN_0_mem_type),
    .io_insts_dispatch_0_priv_vec  (_iq4_io_insts_dispatch_0_priv_vec),
    .io_insts_dispatch_1_prj       (_rename_io_prj_1),
    .io_insts_dispatch_1_prk       (_rename_io_prk_1),
    .io_insts_dispatch_1_rd_valid  (_dr_reg_io_insts_pack_RN_1_rd_valid),
    .io_insts_dispatch_1_prd       (inst_pack_RN_1_prd),
    .io_insts_dispatch_1_imm       (_dr_reg_io_insts_pack_RN_1_imm),
    .io_insts_dispatch_1_rob_index (_iq4_io_insts_dispatch_1_rob_index),
    .io_insts_dispatch_1_mem_type  (_dr_reg_io_insts_pack_RN_1_mem_type),
    .io_insts_dispatch_1_priv_vec  (_iq4_io_insts_dispatch_1_priv_vec),
    .io_prj_ready_0                (_rename_io_prj_ready_0),
    .io_prj_ready_1                (_rename_io_prj_ready_1),
    .io_prk_ready_0                (_rename_io_prk_ready_0),
    .io_prk_ready_1                (_rename_io_prk_ready_1),
    .io_wake_preg_0                (_ir_reg1_io_inst_pack_RF_prd),
    .io_wake_preg_1                (_ir_reg2_io_inst_pack_RF_prd),
    .io_wake_preg_2                (iq_mutual_wake_preg_2),
    .io_wake_preg_3                (_iq4_io_wake_preg_3),
    .io_is_store_cmt_num           (_iq4_io_is_store_cmt_num),
    .io_rob_index_cmt              (_iq4_io_rob_index_cmt),
    .io_issue_ack                  (_iq4_io_issue_ack),
    .io_insts_issue_inst_prj       (_iq4_io_insts_issue_inst_prj),
    .io_insts_issue_inst_prk       (_iq4_io_insts_issue_inst_prk),
    .io_insts_issue_inst_rd_valid  (_iq4_io_insts_issue_inst_rd_valid),
    .io_insts_issue_inst_prd       (_iq4_io_insts_issue_inst_prd),
    .io_insts_issue_inst_imm       (_iq4_io_insts_issue_inst_imm),
    .io_insts_issue_inst_rob_index (_iq4_io_insts_issue_inst_rob_index),
    .io_insts_issue_inst_mem_type  (_iq4_io_insts_issue_inst_mem_type),
    .io_insts_issue_inst_priv_vec  (_iq4_io_insts_issue_inst_priv_vec),
    .io_issue_req                  (_iq4_io_issue_req),
    .io_full                       (_iq4_io_full),
    .io_stall                      (_iq4_io_stall_T),
    .io_flush                      (_iq4_io_flush)
  );
  assign _sel4_io_stall = ~_iq4_io_issue_req | _ir_reg4_io_stall_T_6 | sel4_io_stall_r;
  Order_Select_1 sel4 (
    .io_insts_issue_inst_prj       (_iq4_io_insts_issue_inst_prj),
    .io_insts_issue_inst_prk       (_iq4_io_insts_issue_inst_prk),
    .io_insts_issue_inst_rd_valid  (_iq4_io_insts_issue_inst_rd_valid),
    .io_insts_issue_inst_prd       (_iq4_io_insts_issue_inst_prd),
    .io_insts_issue_inst_imm       (_iq4_io_insts_issue_inst_imm),
    .io_insts_issue_inst_rob_index (_iq4_io_insts_issue_inst_rob_index),
    .io_insts_issue_inst_mem_type  (_iq4_io_insts_issue_inst_mem_type),
    .io_insts_issue_inst_priv_vec  (_iq4_io_insts_issue_inst_priv_vec),
    .io_issue_req                  (_iq4_io_issue_req),
    .io_stall                      (_sel4_io_stall),
    .io_issue_ack                  (_iq4_io_issue_ack),
    .io_inst_issue_inst_prj        (_sel4_io_inst_issue_inst_prj),
    .io_inst_issue_inst_prk        (_sel4_io_inst_issue_inst_prk),
    .io_inst_issue_inst_rd_valid   (_sel4_io_inst_issue_inst_rd_valid),
    .io_inst_issue_inst_prd        (_sel4_io_inst_issue_inst_prd),
    .io_inst_issue_inst_imm        (_sel4_io_inst_issue_inst_imm),
    .io_inst_issue_inst_rob_index  (_sel4_io_inst_issue_inst_rob_index),
    .io_inst_issue_inst_mem_type   (_sel4_io_inst_issue_inst_mem_type),
    .io_inst_issue_inst_priv_vec   (_sel4_io_inst_issue_inst_priv_vec),
    .io_inst_issue_valid           (_sel4_io_inst_issue_valid)
  );
  assign _ir_reg4_io_flush = _rob_io_predict_fail_cmt[7];
  assign _ir_reg4_io_forward_prj_en = _bypass_io_forward_prj_en_2;
  assign _ir_reg4_io_forward_prk_en = _bypass_io_forward_prk_en_2;
  assign _ir_reg4_io_forward_prj_data = _bypass_io_forward_prj_data_2;
  assign _ir_reg4_io_forward_prk_data = _bypass_io_forward_prk_data_2;
  LS_RF_EX_Reg ir_reg4 (
    .clock                      (clock),
    .reset                      (reset),
    .io_flush                   (_ir_reg4_io_flush),
    .io_stall                   (_ir_reg4_io_stall_T_6),
    .io_inst_pack_RF_prj        (_sel4_io_inst_issue_inst_prj),
    .io_inst_pack_RF_prk        (_sel4_io_inst_issue_inst_prk),
    .io_inst_pack_RF_rd_valid   (_sel4_io_inst_issue_inst_rd_valid),
    .io_inst_pack_RF_prd        (_sel4_io_inst_issue_inst_prd),
    .io_inst_pack_RF_imm        (_sel4_io_inst_issue_inst_imm),
    .io_inst_pack_RF_rob_index  (_sel4_io_inst_issue_inst_rob_index),
    .io_inst_pack_RF_mem_type   (_sel4_io_inst_issue_inst_mem_type),
    .io_inst_pack_RF_priv_vec   (_sel4_io_inst_issue_inst_priv_vec),
    .io_inst_pack_RF_inst_valid (_sel4_io_inst_issue_valid),
    .io_src1_RF                 (_ir_reg4_io_src1_RF),
    .io_src2_RF                 (_ir_reg4_io_src2_RF),
    .io_csr_rdata_RF            (_ir_reg4_io_csr_rdata_RF_T_5),
    .io_forward_prj_en          (_ir_reg4_io_forward_prj_en),
    .io_forward_prk_en          (_ir_reg4_io_forward_prk_en),
    .io_forward_prj_data        (_ir_reg4_io_forward_prj_data),
    .io_forward_prk_data        (_ir_reg4_io_forward_prk_data),
    .io_inst_pack_EX_prj        (_ir_reg4_io_inst_pack_EX_prj),
    .io_inst_pack_EX_prk        (_ir_reg4_io_inst_pack_EX_prk),
    .io_inst_pack_EX_rd_valid   (_ir_reg4_io_inst_pack_EX_rd_valid),
    .io_inst_pack_EX_prd        (_ir_reg4_io_inst_pack_EX_prd),
    .io_inst_pack_EX_imm        (_ir_reg4_io_inst_pack_EX_imm),
    .io_inst_pack_EX_rob_index  (_ir_reg4_io_inst_pack_EX_rob_index),
    .io_inst_pack_EX_mem_type   (_ir_reg4_io_inst_pack_EX_mem_type),
    .io_inst_pack_EX_priv_vec   (_ir_reg4_io_inst_pack_EX_priv_vec),
    .io_inst_pack_EX_inst_valid (_ir_reg4_io_inst_pack_EX_inst_valid),
    .io_src1_EX                 (_ir_reg4_io_src1_EX),
    .io_src2_EX                 (_ir_reg4_io_src2_EX),
    .io_csr_rdata_EX            (_ir_reg4_io_csr_rdata_EX)
  );
  assign _rf_io_prd_0 = _ew_reg1_io_inst_pack_WB_prd;
  assign _rf_io_prd_1 = _ew_reg2_io_inst_pack_WB_prd;
  assign _rf_io_prd_3 = _ew_reg4_io_inst_pack_WB_prd;
  assign _rf_io_wdata_0 = _ew_reg1_io_alu_out_WB;
  assign _rf_io_wdata_1 = _ew_reg2_io_alu_out_WB;
  assign _rf_io_wdata_2 = _ew_reg3_io_md_out_WB;
  assign _rf_io_wdata_3 = _ew_reg4_io_mem_rdata_WB;
  assign _rf_io_rf_we_0 = _ew_reg1_io_inst_pack_WB_rd_valid;
  assign _rf_io_rf_we_1 = _ew_reg2_io_inst_pack_WB_rd_valid;
  assign _rf_io_rf_we_3 =
    _ew_reg4_io_inst_pack_WB_rd_valid & ~(_ew_reg4_io_exception_WB[7]);
  Physical_Regfile rf (
    .clock         (clock),
    .reset         (reset),
    .io_prj_0      (_ir_reg1_io_inst_pack_RF_prj),
    .io_prj_1      (_ir_reg2_io_inst_pack_RF_prj),
    .io_prj_2      (_ir_reg3_io_inst_pack_RF_prj),
    .io_prj_3      (_sel4_io_inst_issue_inst_prj),
    .io_prk_0      (_ir_reg1_io_inst_pack_RF_prk),
    .io_prk_1      (_ir_reg2_io_inst_pack_RF_prk),
    .io_prk_2      (_ir_reg3_io_inst_pack_RF_prk),
    .io_prk_3      (_sel4_io_inst_issue_inst_prk),
    .io_prj_data_0 (_rf_io_prj_data_0),
    .io_prj_data_1 (_rf_io_prj_data_1),
    .io_prj_data_2 (_rf_io_prj_data_2),
    .io_prj_data_3 (_ir_reg4_io_src1_RF),
    .io_prk_data_0 (_rf_io_prk_data_0),
    .io_prk_data_1 (_rf_io_prk_data_1),
    .io_prk_data_2 (_rf_io_prk_data_2),
    .io_prk_data_3 (_ir_reg4_io_src2_RF),
    .io_prd_0      (_rf_io_prd_0),
    .io_prd_1      (_rf_io_prd_1),
    .io_prd_2      (_rf_io_prd_2),
    .io_prd_3      (_rf_io_prd_3),
    .io_wdata_0    (_rf_io_wdata_0),
    .io_wdata_1    (_rf_io_wdata_1),
    .io_wdata_2    (_rf_io_wdata_2),
    .io_wdata_3    (_rf_io_wdata_3),
    .io_rf_we_0    (_rf_io_rf_we_0),
    .io_rf_we_1    (_rf_io_rf_we_1),
    .io_rf_we_2    (_rf_io_rf_we_2),
    .io_rf_we_3    (_rf_io_rf_we_3)
  );
  assign _csr_rf_io_raddr = _ir_reg3_io_inst_pack_RF_imm[13:0];
  assign _csr_rf_io_we = _rob_io_csr_we_cmt;
  assign _csr_rf_io_exception = _rob_io_exception_cmt;
  assign _csr_rf_io_pc_exp = _rob_io_pred_pc_cmt;
  assign _csr_rf_io_tlbrd_en = _rob_io_tlbrd_en_cmt;
  CSR_Regfile csr_rf (
    .clock                   (clock),
    .reset                   (reset),
    .io_raddr                (_csr_rf_io_raddr),
    .io_rdata                (_csr_rf_io_rdata),
    .io_waddr                (_csr_rf_io_waddr),
    .io_we                   (_csr_rf_io_we),
    .io_wdata                (_csr_rf_io_wdata),
    .io_exception            (_csr_rf_io_exception),
    .io_badv_exp             (_csr_rf_io_badv_exp),
    .io_is_eret              (_csr_rf_io_is_eret),
    .io_pc_exp               (_csr_rf_io_pc_exp),
    .io_eentry_global        (_csr_rf_io_eentry_global),
    .io_tlbreentry_global    (_csr_rf_io_tlbreentry_global),
    .io_interrupt_vec        (_csr_rf_io_interrupt_vec),
    .io_asid_global          (_csr_rf_io_asid_global),
    .io_plv_global           (_csr_rf_io_plv_global),
    .io_tlbehi_global        (_csr_rf_io_tlbehi_global),
    .io_tlbidx_global        (_csr_rf_io_tlbidx_global),
    .io_crmd_trans           (_csr_rf_io_crmd_trans),
    .io_dmw0_global          (_csr_rf_io_dmw0_global),
    .io_dmw1_global          (_csr_rf_io_dmw1_global),
    .io_tlbentry_global_vppn (_csr_rf_io_tlbentry_global_vppn),
    .io_tlbentry_global_ps   (_csr_rf_io_tlbentry_global_ps),
    .io_tlbentry_global_g    (_csr_rf_io_tlbentry_global_g),
    .io_tlbentry_global_asid (_csr_rf_io_tlbentry_global_asid),
    .io_tlbentry_global_e    (_csr_rf_io_tlbentry_global_e),
    .io_tlbentry_global_ppn0 (_csr_rf_io_tlbentry_global_ppn0),
    .io_tlbentry_global_plv0 (_csr_rf_io_tlbentry_global_plv0),
    .io_tlbentry_global_mat0 (_csr_rf_io_tlbentry_global_mat0),
    .io_tlbentry_global_d0   (_csr_rf_io_tlbentry_global_d0),
    .io_tlbentry_global_v0   (_csr_rf_io_tlbentry_global_v0),
    .io_tlbentry_global_ppn1 (_csr_rf_io_tlbentry_global_ppn1),
    .io_tlbentry_global_plv1 (_csr_rf_io_tlbentry_global_plv1),
    .io_tlbentry_global_mat1 (_csr_rf_io_tlbentry_global_mat1),
    .io_tlbentry_global_d1   (_csr_rf_io_tlbentry_global_d1),
    .io_tlbentry_global_v1   (_csr_rf_io_tlbentry_global_v1),
    .io_tlbentry_in_vppn     (_csr_rf_io_tlbentry_in_vppn),
    .io_tlbentry_in_ps       (_csr_rf_io_tlbentry_in_ps),
    .io_tlbentry_in_g        (_csr_rf_io_tlbentry_in_g),
    .io_tlbentry_in_asid     (_csr_rf_io_tlbentry_in_asid),
    .io_tlbentry_in_e        (_csr_rf_io_tlbentry_in_e),
    .io_tlbentry_in_ppn0     (_csr_rf_io_tlbentry_in_ppn0),
    .io_tlbentry_in_plv0     (_csr_rf_io_tlbentry_in_plv0),
    .io_tlbentry_in_mat0     (_csr_rf_io_tlbentry_in_mat0),
    .io_tlbentry_in_d0       (_csr_rf_io_tlbentry_in_d0),
    .io_tlbentry_in_v0       (_csr_rf_io_tlbentry_in_v0),
    .io_tlbentry_in_ppn1     (_csr_rf_io_tlbentry_in_ppn1),
    .io_tlbentry_in_plv1     (_csr_rf_io_tlbentry_in_plv1),
    .io_tlbentry_in_mat1     (_csr_rf_io_tlbentry_in_mat1),
    .io_tlbentry_in_d1       (_csr_rf_io_tlbentry_in_d1),
    .io_tlbentry_in_v1       (_csr_rf_io_tlbentry_in_v1),
    .io_tlbrd_en             (_csr_rf_io_tlbrd_en),
    .io_tlbsrch_en           (_csr_rf_io_tlbsrch_en),
    .io_llbit_global         (_csr_rf_io_llbit_global),
    .io_llbit_set            (_csr_rf_io_llbit_set),
    .io_llbit_clear          (_csr_rf_io_llbit_clear),
    .io_estat_13             (io_commit_interrupt_type)
  );
  Stable_Counter stable_cnt (
    .clock    (clock),
    .reset    (reset),
    .io_value (_stable_cnt_io_value)
  );
  assign _re_reg1_io_flush = _rob_io_predict_fail_cmt[8];
  RF_EX_Reg re_reg1 (
    .clock                       (clock),
    .reset                       (reset),
    .io_flush                    (_re_reg1_io_flush),
    .io_inst_pack_RF_prj         (_ir_reg1_io_inst_pack_RF_prj),
    .io_inst_pack_RF_prk         (_ir_reg1_io_inst_pack_RF_prk),
    .io_inst_pack_RF_rd_valid    (_ir_reg1_io_inst_pack_RF_rd_valid),
    .io_inst_pack_RF_prd         (_ir_reg1_io_inst_pack_RF_prd),
    .io_inst_pack_RF_imm         (_ir_reg1_io_inst_pack_RF_imm),
    .io_inst_pack_RF_rob_index   (_ir_reg1_io_inst_pack_RF_rob_index),
    .io_inst_pack_RF_alu_op      (_ir_reg1_io_inst_pack_RF_alu_op),
    .io_inst_pack_RF_alu_rs1_sel (_ir_reg1_io_inst_pack_RF_alu_rs1_sel),
    .io_inst_pack_RF_alu_rs2_sel (_ir_reg1_io_inst_pack_RF_alu_rs2_sel),
    .io_inst_pack_RF_pc          (_ir_reg1_io_inst_pack_RF_pc),
    .io_inst_pack_RF_inst_valid  (_ir_reg1_io_inst_pack_RF_inst_valid),
    .io_src1_RF                  (_rf_io_prj_data_0),
    .io_src2_RF                  (_rf_io_prk_data_0),
    .io_inst_pack_EX_prj         (_re_reg1_io_inst_pack_EX_prj),
    .io_inst_pack_EX_prk         (_re_reg1_io_inst_pack_EX_prk),
    .io_inst_pack_EX_rd_valid    (_re_reg1_io_inst_pack_EX_rd_valid),
    .io_inst_pack_EX_prd         (_re_reg1_io_inst_pack_EX_prd),
    .io_inst_pack_EX_imm         (_re_reg1_io_inst_pack_EX_imm),
    .io_inst_pack_EX_rob_index   (_re_reg1_io_inst_pack_EX_rob_index),
    .io_inst_pack_EX_alu_op      (_re_reg1_io_inst_pack_EX_alu_op),
    .io_inst_pack_EX_alu_rs1_sel (_re_reg1_io_inst_pack_EX_alu_rs1_sel),
    .io_inst_pack_EX_alu_rs2_sel (_re_reg1_io_inst_pack_EX_alu_rs2_sel),
    .io_inst_pack_EX_pc          (_re_reg1_io_inst_pack_EX_pc),
    .io_inst_pack_EX_inst_valid  (_re_reg1_io_inst_pack_EX_inst_valid),
    .io_src1_EX                  (_re_reg1_io_src1_EX),
    .io_src2_EX                  (_re_reg1_io_src2_EX)
  );
  assign _re_reg2_io_flush = _rob_io_predict_fail_cmt[8];
  RF_EX_Reg_1 re_reg2 (
    .clock                        (clock),
    .reset                        (reset),
    .io_flush                     (_re_reg2_io_flush),
    .io_inst_pack_RF_prj          (_ir_reg2_io_inst_pack_RF_prj),
    .io_inst_pack_RF_prk          (_ir_reg2_io_inst_pack_RF_prk),
    .io_inst_pack_RF_rd_valid     (_ir_reg2_io_inst_pack_RF_rd_valid),
    .io_inst_pack_RF_prd          (_ir_reg2_io_inst_pack_RF_prd),
    .io_inst_pack_RF_imm          (_ir_reg2_io_inst_pack_RF_imm),
    .io_inst_pack_RF_rob_index    (_ir_reg2_io_inst_pack_RF_rob_index),
    .io_inst_pack_RF_alu_op       (_ir_reg2_io_inst_pack_RF_alu_op),
    .io_inst_pack_RF_alu_rs1_sel  (_ir_reg2_io_inst_pack_RF_alu_rs1_sel),
    .io_inst_pack_RF_alu_rs2_sel  (_ir_reg2_io_inst_pack_RF_alu_rs2_sel),
    .io_inst_pack_RF_pc           (_ir_reg2_io_inst_pack_RF_pc),
    .io_inst_pack_RF_br_type      (_ir_reg2_io_inst_pack_RF_br_type),
    .io_inst_pack_RF_predict_jump (_ir_reg2_io_inst_pack_RF_predict_jump),
    .io_inst_pack_RF_pred_npc     (_ir_reg2_io_inst_pack_RF_pred_npc),
    .io_inst_pack_RF_inst_valid   (_ir_reg2_io_inst_pack_RF_inst_valid),
    .io_src1_RF                   (_rf_io_prj_data_1),
    .io_src2_RF                   (_rf_io_prk_data_1),
    .io_inst_pack_EX_prj          (_re_reg2_io_inst_pack_EX_prj),
    .io_inst_pack_EX_prk          (_re_reg2_io_inst_pack_EX_prk),
    .io_inst_pack_EX_rd_valid     (_re_reg2_io_inst_pack_EX_rd_valid),
    .io_inst_pack_EX_prd          (_re_reg2_io_inst_pack_EX_prd),
    .io_inst_pack_EX_imm          (_re_reg2_io_inst_pack_EX_imm),
    .io_inst_pack_EX_rob_index    (_re_reg2_io_inst_pack_EX_rob_index),
    .io_inst_pack_EX_alu_op       (_re_reg2_io_inst_pack_EX_alu_op),
    .io_inst_pack_EX_alu_rs1_sel  (_re_reg2_io_inst_pack_EX_alu_rs1_sel),
    .io_inst_pack_EX_alu_rs2_sel  (_re_reg2_io_inst_pack_EX_alu_rs2_sel),
    .io_inst_pack_EX_pc           (_re_reg2_io_inst_pack_EX_pc),
    .io_inst_pack_EX_br_type      (_re_reg2_io_inst_pack_EX_br_type),
    .io_inst_pack_EX_predict_jump (_re_reg2_io_inst_pack_EX_predict_jump),
    .io_inst_pack_EX_pred_npc     (_re_reg2_io_inst_pack_EX_pred_npc),
    .io_inst_pack_EX_inst_valid   (_re_reg2_io_inst_pack_EX_inst_valid),
    .io_src1_EX                   (_re_reg2_io_src1_EX),
    .io_src2_EX                   (_re_reg2_io_src2_EX)
  );
  assign _re_reg3_io_flush = _rob_io_predict_fail_cmt[8];
  assign _re_reg3_io_stall = _mdu_io_busy_18;
  RF_EX_Reg_2 re_reg3 (
    .clock                      (clock),
    .reset                      (reset),
    .io_flush                   (_re_reg3_io_flush),
    .io_stall                   (_re_reg3_io_stall),
    .io_inst_pack_RF_rd_valid   (_ir_reg3_io_inst_pack_RF_rd_valid),
    .io_inst_pack_RF_prd        (_ir_reg3_io_inst_pack_RF_prd),
    .io_inst_pack_RF_imm        (_ir_reg3_io_inst_pack_RF_imm),
    .io_inst_pack_RF_rob_index  (_ir_reg3_io_inst_pack_RF_rob_index),
    .io_inst_pack_RF_priv_vec   (_ir_reg3_io_inst_pack_RF_priv_vec),
    .io_inst_pack_RF_alu_op     (_ir_reg3_io_inst_pack_RF_alu_op),
    .io_inst_pack_RF_inst_valid (_ir_reg3_io_inst_pack_RF_inst_valid),
    .io_src1_RF                 (_rf_io_prj_data_2),
    .io_src2_RF                 (_rf_io_prk_data_2),
    .io_csr_rdata_RF            (_csr_rf_io_rdata),
    .io_inst_pack_EX_rd_valid   (_re_reg3_io_inst_pack_EX_rd_valid),
    .io_inst_pack_EX_prd        (_re_reg3_io_inst_pack_EX_prd),
    .io_inst_pack_EX_imm        (_re_reg3_io_inst_pack_EX_imm),
    .io_inst_pack_EX_rob_index  (_re_reg3_io_inst_pack_EX_rob_index),
    .io_inst_pack_EX_priv_vec   (_re_reg3_io_inst_pack_EX_priv_vec),
    .io_inst_pack_EX_alu_op     (_re_reg3_io_inst_pack_EX_alu_op),
    .io_inst_pack_EX_inst_valid (_re_reg3_io_inst_pack_EX_inst_valid),
    .io_src1_EX                 (_re_reg3_io_src1_EX),
    .io_src2_EX                 (_re_reg3_io_src2_EX),
    .io_csr_rdata_EX            (_re_reg3_io_csr_rdata_EX)
  );
  assign _re_reg4_io_flush =
    _rob_io_predict_fail_cmt[8] | ~_re_reg4_io_stall_T_2 & _sb_io_st_cmt_valid
    & (|(_ir_reg4_io_inst_pack_EX_mem_type[4:3]));
  RF_EX_Reg_3 re_reg4 (
    .clock                      (clock),
    .reset                      (reset),
    .io_flush                   (_re_reg4_io_flush),
    .io_stall                   (_re_reg4_io_stall_T_2),
    .io_inst_pack_RF_rd_valid   (_ir_reg4_io_inst_pack_EX_rd_valid),
    .io_inst_pack_RF_prd        (_ir_reg4_io_inst_pack_EX_prd),
    .io_inst_pack_RF_imm        (_ir_reg4_io_inst_pack_EX_imm),
    .io_inst_pack_RF_rob_index  (_ir_reg4_io_inst_pack_EX_rob_index),
    .io_inst_pack_RF_mem_type   (_ir_reg4_io_inst_pack_EX_mem_type),
    .io_inst_pack_RF_priv_vec   (_ir_reg4_io_inst_pack_EX_priv_vec),
    .io_inst_pack_RF_inst_valid (_ir_reg4_io_inst_pack_EX_inst_valid),
    .io_src1_RF                 (_re_reg4_io_src1_RF_T_1),
    .io_src2_RF                 (_re_reg4_io_src2_RF_T),
    .io_inst_pack_EX_rd_valid   (_re_reg4_io_inst_pack_EX_rd_valid),
    .io_inst_pack_EX_prd        (_re_reg4_io_inst_pack_EX_prd),
    .io_inst_pack_EX_imm        (_re_reg4_io_inst_pack_EX_imm),
    .io_inst_pack_EX_rob_index  (_re_reg4_io_inst_pack_EX_rob_index),
    .io_inst_pack_EX_mem_type   (_re_reg4_io_inst_pack_EX_mem_type),
    .io_inst_pack_EX_priv_vec   (_re_reg4_io_inst_pack_EX_priv_vec),
    .io_inst_pack_EX_inst_valid (_re_reg4_io_inst_pack_EX_inst_valid),
    .io_src1_EX                 (_re_reg4_io_src1_EX),
    .io_src2_EX                 (_re_reg4_io_src2_EX)
  );
  assign _alu1_io_src2 = _GEN[_re_reg1_io_inst_pack_EX_alu_rs2_sel];
  ALU alu1 (
    .io_src1    (_alu1_io_src1_T_2),
    .io_src2    (_alu1_io_src2),
    .io_alu_op  (_re_reg1_io_inst_pack_EX_alu_op),
    .io_alu_out (_alu1_io_alu_out)
  );
  assign _ew_reg1_io_flush = _rob_io_predict_fail_cmt[9];
  assign _ew_reg1_io_is_ucread_EX =
    _re_reg1_io_inst_pack_EX_alu_rs2_sel == 2'h2
    | (&_re_reg1_io_inst_pack_EX_alu_rs2_sel);
  FU1_EX_WB_Reg ew_reg1 (
    .clock                      (clock),
    .reset                      (reset),
    .io_flush                   (_ew_reg1_io_flush),
    .io_inst_pack_EX_rd_valid   (_re_reg1_io_inst_pack_EX_rd_valid),
    .io_inst_pack_EX_prd        (_re_reg1_io_inst_pack_EX_prd),
    .io_inst_pack_EX_rob_index  (_re_reg1_io_inst_pack_EX_rob_index),
    .io_inst_pack_EX_inst_valid (_re_reg1_io_inst_pack_EX_inst_valid),
    .io_alu_out_EX              (_alu1_io_alu_out),
    .io_is_ucread_EX            (_ew_reg1_io_is_ucread_EX),
    .io_inst_pack_WB_rd_valid   (_ew_reg1_io_inst_pack_WB_rd_valid),
    .io_inst_pack_WB_prd        (_ew_reg1_io_inst_pack_WB_prd),
    .io_inst_pack_WB_rob_index  (_ew_reg1_io_inst_pack_WB_rob_index),
    .io_inst_pack_WB_inst_valid (_ew_reg1_io_inst_pack_WB_inst_valid),
    .io_alu_out_WB              (_ew_reg1_io_alu_out_WB),
    .io_is_ucread_WB            (_ew_reg1_io_is_ucread_WB)
  );
  assign _alu2_io_src2 = _GEN_0[_re_reg2_io_inst_pack_EX_alu_rs2_sel];
  ALU alu2 (
    .io_src1    (_alu2_io_src1_T_2),
    .io_src2    (_alu2_io_src2),
    .io_alu_op  (_re_reg2_io_inst_pack_EX_alu_op),
    .io_alu_out (_alu2_io_alu_out)
  );
  Branch br (
    .io_br_type       (_re_reg2_io_inst_pack_EX_br_type),
    .io_src1          (_br_io_src1_T),
    .io_src2          (_br_io_src2_T),
    .io_pc_ex         (_re_reg2_io_inst_pack_EX_pc),
    .io_imm_ex        (_re_reg2_io_inst_pack_EX_imm),
    .io_predict_jump  (_re_reg2_io_inst_pack_EX_predict_jump),
    .io_pred_npc      (_re_reg2_io_inst_pack_EX_pred_npc),
    .io_real_jump     (_br_io_real_jump),
    .io_predict_fail  (_br_io_predict_fail),
    .io_branch_target (_br_io_branch_target)
  );
  assign _ew_reg2_io_flush = _rob_io_predict_fail_cmt[9];
  FU2_EX_WB_Reg ew_reg2 (
    .clock                      (clock),
    .reset                      (reset),
    .io_flush                   (_ew_reg2_io_flush),
    .io_inst_pack_EX_rd_valid   (_re_reg2_io_inst_pack_EX_rd_valid),
    .io_inst_pack_EX_prd        (_re_reg2_io_inst_pack_EX_prd),
    .io_inst_pack_EX_rob_index  (_re_reg2_io_inst_pack_EX_rob_index),
    .io_inst_pack_EX_inst_valid (_re_reg2_io_inst_pack_EX_inst_valid),
    .io_alu_out_EX              (_alu2_io_alu_out),
    .io_predict_fail_EX         (_br_io_predict_fail),
    .io_branch_target_EX        (_br_io_branch_target),
    .io_real_jump_EX            (_br_io_real_jump),
    .io_inst_pack_WB_rd_valid   (_ew_reg2_io_inst_pack_WB_rd_valid),
    .io_inst_pack_WB_prd        (_ew_reg2_io_inst_pack_WB_prd),
    .io_inst_pack_WB_rob_index  (_ew_reg2_io_inst_pack_WB_rob_index),
    .io_inst_pack_WB_inst_valid (_ew_reg2_io_inst_pack_WB_inst_valid),
    .io_alu_out_WB              (_ew_reg2_io_alu_out_WB),
    .io_predict_fail_WB         (_ew_reg2_io_predict_fail_WB),
    .io_branch_target_WB        (_ew_reg2_io_branch_target_WB),
    .io_real_jump_WB            (_ew_reg2_io_real_jump_WB)
  );
  assign _mdu_io_md_op = {1'h0, _re_reg3_io_inst_pack_EX_alu_op};
  MDU mdu (
    .clock      (clock),
    .reset      (reset),
    .io_src1    (_re_reg3_io_src1_EX),
    .io_src2    (_re_reg3_io_src2_EX),
    .io_md_op   (_mdu_io_md_op),
    .io_mul_out (_mdu_io_mul_out),
    .io_div_out (_mdu_io_div_out),
    .io_busy_16 (_mdu_io_busy_16),
    .io_busy_17 (_mdu_io_busy_17),
    .io_busy_18 (_mdu_io_busy_18),
    .io_busy_19 (_mdu_io_busy_19),
    .io_busy_20 (_mdu_io_busy_20),
    .io_busy_21 (io_commit_stall_by_div)
  );
  assign _md_ex1_ex2_reg_io_flush = _rob_io_predict_fail_cmt[6];
  MD_EX1_EX2_Reg md_ex1_ex2_reg (
    .clock                       (clock),
    .reset                       (reset),
    .io_flush                    (_md_ex1_ex2_reg_io_flush),
    .io_stall                    (_mdu_io_busy_19),
    .io_inst_pack_EX1_rd_valid   (_re_reg3_io_inst_pack_EX_rd_valid),
    .io_inst_pack_EX1_prd        (_re_reg3_io_inst_pack_EX_prd),
    .io_inst_pack_EX1_rob_index  (_re_reg3_io_inst_pack_EX_rob_index),
    .io_inst_pack_EX1_priv_vec   (_re_reg3_io_inst_pack_EX_priv_vec),
    .io_inst_pack_EX1_alu_op     (_re_reg3_io_inst_pack_EX_alu_op),
    .io_inst_pack_EX1_inst_valid (_re_reg3_io_inst_pack_EX_inst_valid),
    .io_csr_wdata_EX1            (csr_wdata),
    .io_csr_rdata_EX1            (_re_reg3_io_csr_rdata_EX),
    .io_inst_pack_EX2_rd_valid   (_md_ex1_ex2_reg_io_inst_pack_EX2_rd_valid),
    .io_inst_pack_EX2_prd        (_md_ex1_ex2_reg_io_inst_pack_EX2_prd),
    .io_inst_pack_EX2_rob_index  (_md_ex1_ex2_reg_io_inst_pack_EX2_rob_index),
    .io_inst_pack_EX2_priv_vec   (_md_ex1_ex2_reg_io_inst_pack_EX2_priv_vec),
    .io_inst_pack_EX2_alu_op     (_md_ex1_ex2_reg_io_inst_pack_EX2_alu_op),
    .io_inst_pack_EX2_inst_valid (_md_ex1_ex2_reg_io_inst_pack_EX2_inst_valid),
    .io_csr_wdata_EX2            (_md_ex1_ex2_reg_io_csr_wdata_EX2),
    .io_csr_rdata_EX2            (_md_ex1_ex2_reg_io_csr_rdata_EX2)
  );
  assign _md_ex2_ex3_reg_io_flush = _rob_io_predict_fail_cmt[6];
  MD_EX1_EX2_Reg md_ex2_ex3_reg (
    .clock                       (clock),
    .reset                       (reset),
    .io_flush                    (_md_ex2_ex3_reg_io_flush),
    .io_stall                    (_mdu_io_busy_20),
    .io_inst_pack_EX1_rd_valid   (_md_ex1_ex2_reg_io_inst_pack_EX2_rd_valid),
    .io_inst_pack_EX1_prd        (_md_ex1_ex2_reg_io_inst_pack_EX2_prd),
    .io_inst_pack_EX1_rob_index  (_md_ex1_ex2_reg_io_inst_pack_EX2_rob_index),
    .io_inst_pack_EX1_priv_vec   (_md_ex1_ex2_reg_io_inst_pack_EX2_priv_vec),
    .io_inst_pack_EX1_alu_op     (_md_ex1_ex2_reg_io_inst_pack_EX2_alu_op),
    .io_inst_pack_EX1_inst_valid (_md_ex1_ex2_reg_io_inst_pack_EX2_inst_valid),
    .io_csr_wdata_EX1            (_md_ex1_ex2_reg_io_csr_wdata_EX2),
    .io_csr_rdata_EX1            (_md_ex1_ex2_reg_io_csr_rdata_EX2),
    .io_inst_pack_EX2_rd_valid   (_md_ex2_ex3_reg_io_inst_pack_EX2_rd_valid),
    .io_inst_pack_EX2_prd        (_md_ex2_ex3_reg_io_inst_pack_EX2_prd),
    .io_inst_pack_EX2_rob_index  (_md_ex2_ex3_reg_io_inst_pack_EX2_rob_index),
    .io_inst_pack_EX2_priv_vec   (_md_ex2_ex3_reg_io_inst_pack_EX2_priv_vec),
    .io_inst_pack_EX2_alu_op     (_md_ex2_ex3_reg_io_inst_pack_EX2_alu_op),
    .io_inst_pack_EX2_inst_valid (_md_ex2_ex3_reg_io_inst_pack_EX2_inst_valid),
    .io_csr_wdata_EX2            (_md_ex2_ex3_reg_io_csr_wdata_EX2),
    .io_csr_rdata_EX2            (_md_ex2_ex3_reg_io_csr_rdata_EX2)
  );
  assign _ew_reg3_io_flush = _rob_io_predict_fail_cmt[9] | _mdu_io_busy_19;
  MD_EX_WB_Reg ew_reg3 (
    .clock                      (clock),
    .reset                      (reset),
    .io_flush                   (_ew_reg3_io_flush),
    .io_inst_pack_EX_rd_valid   (_md_ex2_ex3_reg_io_inst_pack_EX2_rd_valid),
    .io_inst_pack_EX_prd        (_md_ex2_ex3_reg_io_inst_pack_EX2_prd),
    .io_inst_pack_EX_rob_index  (_md_ex2_ex3_reg_io_inst_pack_EX2_rob_index),
    .io_inst_pack_EX_inst_valid (_md_ex2_ex3_reg_io_inst_pack_EX2_inst_valid),
    .io_md_out_EX               (_ew_reg3_io_md_out_EX_T_4),
    .io_csr_wdata_EX            (_md_ex2_ex3_reg_io_csr_wdata_EX2),
    .io_inst_pack_WB_rd_valid   (_rf_io_rf_we_2),
    .io_inst_pack_WB_prd        (_rf_io_prd_2),
    .io_inst_pack_WB_rob_index  (_ew_reg3_io_inst_pack_WB_rob_index),
    .io_inst_pack_WB_inst_valid (_ew_reg3_io_inst_pack_WB_inst_valid),
    .io_md_out_WB               (_ew_reg3_io_md_out_WB),
    .io_csr_wdata_WB            (_ew_reg3_io_csr_wdata_WB)
  );
  Exception_LS exception_ls (
    .io_addr_EX      (_re_reg4_io_src1_EX),
    .io_mem_type_EX  (_re_reg4_io_inst_pack_EX_mem_type),
    .io_exception_ls (_exception_ls_io_exception_ls)
  );
  assign _mmu_io_tlbfill_idx = _stable_cnt_io_value[3:0];
  assign _mmu_io_tlbfill_en = _rob_io_tlbfill_en_cmt;
  assign _mmu_io_i_valid = ~reset;
  assign _mmu_io_d_rvalid = _ir_reg4_io_inst_pack_EX_mem_type[3];
  assign _mmu_io_d_wvalid = _ir_reg4_io_inst_pack_EX_mem_type[4];
  assign _mmu_io_d_vaddr = 32'(_re_reg4_io_src1_RF_T + _ir_reg4_io_csr_rdata_EX);
  MMU mmu (
    .clock               (clock),
    .reset               (reset),
    .io_csr_asid         (_csr_rf_io_asid_global),
    .io_csr_plv          (_csr_rf_io_plv_global),
    .io_csr_crmd_trans   (_csr_rf_io_crmd_trans),
    .io_csr_dmw0         (_csr_rf_io_dmw0_global),
    .io_csr_dmw1         (_csr_rf_io_dmw1_global),
    .io_csr_tlbehi       (_csr_rf_io_tlbehi_global),
    .io_tlbsrch_idx      (_mmu_io_tlbsrch_idx),
    .io_tlbsrch_hit      (_mmu_io_tlbsrch_hit),
    .io_csr_tlbidx       (_csr_rf_io_tlbidx_global),
    .io_tlbrd_entry_vppn (_mmu_io_tlbrd_entry_vppn),
    .io_tlbrd_entry_ps   (_mmu_io_tlbrd_entry_ps),
    .io_tlbrd_entry_g    (_mmu_io_tlbrd_entry_g),
    .io_tlbrd_entry_asid (_mmu_io_tlbrd_entry_asid),
    .io_tlbrd_entry_e    (_mmu_io_tlbrd_entry_e),
    .io_tlbrd_entry_ppn0 (_mmu_io_tlbrd_entry_ppn0),
    .io_tlbrd_entry_plv0 (_mmu_io_tlbrd_entry_plv0),
    .io_tlbrd_entry_mat0 (_mmu_io_tlbrd_entry_mat0),
    .io_tlbrd_entry_d0   (_mmu_io_tlbrd_entry_d0),
    .io_tlbrd_entry_v0   (_mmu_io_tlbrd_entry_v0),
    .io_tlbrd_entry_ppn1 (_mmu_io_tlbrd_entry_ppn1),
    .io_tlbrd_entry_plv1 (_mmu_io_tlbrd_entry_plv1),
    .io_tlbrd_entry_mat1 (_mmu_io_tlbrd_entry_mat1),
    .io_tlbrd_entry_d1   (_mmu_io_tlbrd_entry_d1),
    .io_tlbrd_entry_v1   (_mmu_io_tlbrd_entry_v1),
    .io_tlbwr_entry_vppn (_csr_rf_io_tlbentry_global_vppn),
    .io_tlbwr_entry_ps   (_csr_rf_io_tlbentry_global_ps),
    .io_tlbwr_entry_g    (_csr_rf_io_tlbentry_global_g),
    .io_tlbwr_entry_asid (_csr_rf_io_tlbentry_global_asid),
    .io_tlbwr_entry_e    (_csr_rf_io_tlbentry_global_e),
    .io_tlbwr_entry_ppn0 (_csr_rf_io_tlbentry_global_ppn0),
    .io_tlbwr_entry_plv0 (_csr_rf_io_tlbentry_global_plv0),
    .io_tlbwr_entry_mat0 (_csr_rf_io_tlbentry_global_mat0),
    .io_tlbwr_entry_d0   (_csr_rf_io_tlbentry_global_d0),
    .io_tlbwr_entry_v0   (_csr_rf_io_tlbentry_global_v0),
    .io_tlbwr_entry_ppn1 (_csr_rf_io_tlbentry_global_ppn1),
    .io_tlbwr_entry_plv1 (_csr_rf_io_tlbentry_global_plv1),
    .io_tlbwr_entry_mat1 (_csr_rf_io_tlbentry_global_mat1),
    .io_tlbwr_entry_d1   (_csr_rf_io_tlbentry_global_d1),
    .io_tlbwr_entry_v1   (_csr_rf_io_tlbentry_global_v1),
    .io_tlbwr_en         (_mmu_io_tlbwr_en),
    .io_tlbfill_idx      (_mmu_io_tlbfill_idx),
    .io_tlbfill_en       (_mmu_io_tlbfill_en),
    .io_invtlb_en        (_mmu_io_invtlb_en),
    .io_invtlb_op        (_mmu_io_invtlb_op),
    .io_invtlb_asid      (_mmu_io_invtlb_asid),
    .io_invtlb_vaddr     (_mmu_io_invtlb_vaddr),
    .io_i_valid          (_mmu_io_i_valid),
    .io_i_vaddr          (_pc_io_pc_PF_7),
    .io_i_paddr          (_mmu_io_i_paddr),
    .io_i_uncache        (_icache_io_uncache_IF),
    .io_i_exception      (_mmu_io_i_exception),
    .io_i_stall          (_pi_reg_io_stall_T_1),
    .io_d_rvalid         (_mmu_io_d_rvalid),
    .io_d_wvalid         (_mmu_io_d_wvalid),
    .io_d_vaddr          (_mmu_io_d_vaddr),
    .io_d_paddr          (_mmu_io_d_paddr),
    .io_d_uncache        (_mmu_io_d_uncache),
    .io_d_exception      (_mmu_io_d_exception),
    .io_d_stall          (_re_reg4_io_stall_T_2)
  );
  assign _sb_io_is_store_num_cmt = _rob_io_is_store_num_cmt;
  assign _sb_io_dcache_miss = _dcache_io_cache_miss_MEM_4;
  assign _sb_io_flush = _rob_io_predict_fail_cmt[6];
  assign _sb_io_em_stall = _dcache_io_cache_miss_MEM_4;
  SB sb (
    .clock               (clock),
    .reset               (reset),
    .io_full             (_sb_io_full),
    .io_addr_ex          (_mmu_io_d_paddr),
    .io_st_data_ex       (_re_reg4_io_src2_EX),
    .io_mem_type_ex      (_sb_io_mem_type_ex_T),
    .io_uncache_ex       (_mmu_io_d_uncache),
    .io_is_store_num_cmt (_sb_io_is_store_num_cmt),
    .io_st_cmt_valid     (_sb_io_st_cmt_valid),
    .io_dcache_miss      (_sb_io_dcache_miss),
    .io_st_addr_cmt      (_sb_io_st_addr_cmt),
    .io_st_data_cmt      (_sb_io_st_data_cmt),
    .io_st_wlen_cmt      (_sb_io_st_wlen_cmt),
    .io_is_uncache_cmt   (_sb_io_is_uncache_cmt),
    .io_flush            (_sb_io_flush),
    .io_ld_data_mem      (_sb_io_ld_data_mem),
    .io_ld_hit_0         (_sb_io_ld_hit_0),
    .io_ld_hit_1         (_sb_io_ld_hit_1),
    .io_ld_hit_2         (_sb_io_ld_hit_2),
    .io_ld_hit_3         (_sb_io_ld_hit_3),
    .io_em_stall         (_sb_io_em_stall)
  );
  assign _dcache_io_cacop_en =
    ~_sb_io_st_cmt_valid & _ir_reg4_io_inst_pack_EX_priv_vec[0]
    & _ir_reg4_io_inst_pack_EX_imm[2:0] == 3'h1;
  assign _dcache_io_cacop_op = _ir_reg4_io_inst_pack_EX_imm[4:3];
  assign _dcache_io_flush = _rob_io_predict_fail_cmt[6];
  DCache dcache (
    .clock                  (clock),
    .reset                  (reset),
    .io_addr_RF             (_dcache_io_addr_RF_T),
    .io_mem_type_RF         (_dcache_io_mem_type_RF_T_6),
    .io_wdata_RF            (_dcache_io_wdata_RF_T),
    .io_store_cmt_RF        (_sb_io_st_cmt_valid),
    .io_rob_index_EX        (_re_reg4_io_inst_pack_EX_rob_index),
    .io_paddr_EX            (_mmu_io_d_paddr),
    .io_uncache_EX          (_mmu_io_d_uncache),
    .io_cache_miss_MEM_3    (_dcache_io_cache_miss_MEM_3),
    .io_cache_miss_MEM_4    (_dcache_io_cache_miss_MEM_4),
    .io_rdata_MEM           (_dcache_io_rdata_MEM),
    .io_cache_miss_iq_0     (_iq1_io_dcache_miss),
    .io_cache_miss_iq_1     (_iq2_io_dcache_miss),
    .io_cache_miss_iq_2     (_iq3_io_dcache_miss),
    .io_rob_index_CMT       (_dcache_io_rob_index_CMT),
    .io_cacop_en            (_dcache_io_cacop_en),
    .io_cacop_op            (_dcache_io_cacop_op),
    .io_flush               (_dcache_io_flush),
    .io_d_araddr            (_arb_io_d_araddr),
    .io_d_rvalid            (_arb_io_d_rvalid),
    .io_d_rready            (_arb_io_d_rready),
    .io_d_rdata             (_arb_io_d_rdata),
    .io_d_rlast             (_arb_io_d_rlast),
    .io_d_rsize             (_arb_io_d_rsize),
    .io_d_rlen              (_arb_io_d_rlen),
    .io_d_awaddr            (_arb_io_d_awaddr),
    .io_d_wdata             (_arb_io_d_wdata),
    .io_d_wvalid            (_arb_io_d_wvalid),
    .io_d_wready            (_arb_io_d_wready),
    .io_d_wlast             (_arb_io_d_wlast),
    .io_d_wstrb             (_arb_io_d_wstrb),
    .io_d_wsize             (_arb_io_d_wsize),
    .io_d_wlen              (_arb_io_d_wlen),
    .io_d_bvalid            (_arb_io_d_bvalid),
    .io_d_bready            (_arb_io_d_bready),
    .io_commit_dcache_visit (io_commit_dcache_visit),
    .io_commit_dcache_miss  (io_commit_dcache_miss)
  );
  assign _ls_ex_mem_reg_io_flush =
    _rob_io_predict_fail_cmt[6] | ~_dcache_io_cache_miss_MEM_4 & _sb_io_full
    & _re_reg4_io_inst_pack_EX_mem_type[4];
  assign _ls_ex_mem_reg_io_is_ucread_EX = _re_reg4_io_src1_EX[31:28] == 4'hA;
  LS_EX_MEM_Reg ls_ex_mem_reg (
    .clock                       (clock),
    .reset                       (reset),
    .io_flush                    (_ls_ex_mem_reg_io_flush),
    .io_stall                    (_dcache_io_cache_miss_MEM_4),
    .io_inst_pack_EX_rd_valid    (_re_reg4_io_inst_pack_EX_rd_valid),
    .io_inst_pack_EX_prd         (_re_reg4_io_inst_pack_EX_prd),
    .io_inst_pack_EX_rob_index   (_re_reg4_io_inst_pack_EX_rob_index),
    .io_inst_pack_EX_mem_type    (_re_reg4_io_inst_pack_EX_mem_type),
    .io_inst_pack_EX_priv_vec    (_re_reg4_io_inst_pack_EX_priv_vec),
    .io_inst_pack_EX_inst_valid  (_re_reg4_io_inst_pack_EX_inst_valid),
    .io_is_ucread_EX             (_ls_ex_mem_reg_io_is_ucread_EX),
    .io_src1_EX                  (_re_reg4_io_src1_EX),
    .io_llbit_EX                 (_csr_rf_io_llbit_global),
    .io_prd_EX_0                 (_GEN_1),
    .io_prd_EX_1                 (_GEN_1),
    .io_prd_EX_2                 (_GEN_1),
    .io_exception_EX             (exception_EX),
    .io_inst_pack_MEM_rd_valid   (_ls_ex_mem_reg_io_inst_pack_MEM_rd_valid),
    .io_inst_pack_MEM_prd        (_ls_ex_mem_reg_io_inst_pack_MEM_prd),
    .io_inst_pack_MEM_rob_index  (_ls_ex_mem_reg_io_inst_pack_MEM_rob_index),
    .io_inst_pack_MEM_mem_type   (_ls_ex_mem_reg_io_inst_pack_MEM_mem_type),
    .io_inst_pack_MEM_priv_vec   (_ls_ex_mem_reg_io_inst_pack_MEM_priv_vec),
    .io_inst_pack_MEM_inst_valid (_ls_ex_mem_reg_io_inst_pack_MEM_inst_valid),
    .io_is_ucread_MEM            (_ls_ex_mem_reg_io_is_ucread_MEM),
    .io_src1_MEM                 (_ls_ex_mem_reg_io_src1_MEM),
    .io_llbit_MEM                (_ls_ex_mem_reg_io_llbit_MEM),
    .io_prd_MEM_0                (_ls_ex_mem_reg_io_prd_MEM_0),
    .io_prd_MEM_1                (_ls_ex_mem_reg_io_prd_MEM_1),
    .io_prd_MEM_2                (_ls_ex_mem_reg_io_prd_MEM_2),
    .io_exception_MEM            (_ls_ex_mem_reg_io_exception_MEM)
  );
  assign _ew_reg4_io_flush = _rob_io_predict_fail_cmt[9] | _dcache_io_cache_miss_MEM_3;
  LS_EX2_WB_Reg ew_reg4 (
    .clock                       (clock),
    .reset                       (reset),
    .io_flush                    (_ew_reg4_io_flush),
    .io_inst_pack_EX2_rd_valid   (_ls_ex_mem_reg_io_inst_pack_MEM_rd_valid),
    .io_inst_pack_EX2_prd        (_ls_ex_mem_reg_io_inst_pack_MEM_prd),
    .io_inst_pack_EX2_rob_index  (_ls_ex_mem_reg_io_inst_pack_MEM_rob_index),
    .io_inst_pack_EX2_inst_valid (_ls_ex_mem_reg_io_inst_pack_MEM_inst_valid),
    .io_vaddr_EX2                (_ls_ex_mem_reg_io_src1_MEM),
    .io_exception_EX2            (_ls_ex_mem_reg_io_exception_MEM),
    .io_mem_rdata_EX2            (ls_wb_data),
    .io_is_ucread_EX2            (_ls_ex_mem_reg_io_is_ucread_MEM),
    .io_inst_pack_WB_rd_valid    (_ew_reg4_io_inst_pack_WB_rd_valid),
    .io_inst_pack_WB_prd         (_ew_reg4_io_inst_pack_WB_prd),
    .io_inst_pack_WB_rob_index   (_ew_reg4_io_inst_pack_WB_rob_index),
    .io_inst_pack_WB_inst_valid  (_ew_reg4_io_inst_pack_WB_inst_valid),
    .io_vaddr_WB                 (_ew_reg4_io_vaddr_WB),
    .io_exception_WB             (_ew_reg4_io_exception_WB),
    .io_mem_rdata_WB             (_ew_reg4_io_mem_rdata_WB),
    .io_is_ucread_WB             (_ew_reg4_io_is_ucread_WB)
  );
  assign _rob_io_is_store_dp_0 = _dr_reg_io_insts_pack_RN_0_mem_type[4];
  assign _rob_io_is_store_dp_1 = _dr_reg_io_insts_pack_RN_1_mem_type[4];
  assign _rob_io_pred_update_en_dp_0 = |_dr_reg_io_insts_pack_RN_0_br_type;
  assign _rob_io_pred_update_en_dp_1 = |_dr_reg_io_insts_pack_RN_1_br_type;
  assign _rob_io_interrupt_vec = {1'h0, _csr_rf_io_interrupt_vec};
  assign _rob_io_csr_addr_ex = _re_reg3_io_inst_pack_EX_imm[13:0];
  assign _rob_io_invtlb_op_ex = _re_reg3_io_inst_pack_EX_imm[4:0];
  assign _rob_io_invtlb_asid_ex = _re_reg3_io_src1_EX[9:0];
  ROB rob (
    .clock                     (clock),
    .reset                     (reset),
    .io_inst_valid_dp_0        (_dr_reg_io_insts_pack_RN_0_inst_valid),
    .io_rd_dp_0                (_dr_reg_io_insts_pack_RN_0_rd),
    .io_rd_dp_1                (_dr_reg_io_insts_pack_RN_1_rd),
    .io_rd_valid_dp_0          (_dr_reg_io_insts_pack_RN_0_rd_valid),
    .io_rd_valid_dp_1          (_dr_reg_io_insts_pack_RN_1_rd_valid),
    .io_prd_dp_0               (_rename_io_prd_0),
    .io_prd_dp_1               (_rename_io_prd_1),
    .io_pprd_dp_0              (_rename_io_pprd_0),
    .io_pprd_dp_1              (_rename_io_pprd_1),
    .io_rob_index_dp_0         (_rob_io_rob_index_dp_0),
    .io_rob_index_dp_1         (_rob_io_rob_index_dp_1),
    .io_pc_dp_0                (_dr_reg_io_insts_pack_RN_0_pc),
    .io_pc_dp_1                (_dr_reg_io_insts_pack_RN_1_pc),
    .io_is_store_dp_0          (_rob_io_is_store_dp_0),
    .io_is_store_dp_1          (_rob_io_is_store_dp_1),
    .io_br_type_pred_dp_0      (br_type_dp_0),
    .io_br_type_pred_dp_1      (br_type_dp_1),
    .io_pred_update_en_dp_0    (_rob_io_pred_update_en_dp_0),
    .io_pred_update_en_dp_1    (_rob_io_pred_update_en_dp_1),
    .io_priv_vec_dp_0          (_dr_reg_io_insts_pack_RN_0_priv_vec),
    .io_priv_vec_dp_1          (_dr_reg_io_insts_pack_RN_1_priv_vec),
    .io_exception_dp_0         (_dr_reg_io_insts_pack_RN_0_exception),
    .io_exception_dp_1         (_dr_reg_io_insts_pack_RN_1_exception),
    .io_inst_dp_0              (_dr_reg_io_inst_RN_0),
    .io_inst_dp_1              (_dr_reg_io_inst_RN_1),
    .io_full_2                 (_rob_io_full_2),
    .io_full_3                 (_rob_io_full_3),
    .io_full_5                 (_rob_io_full_5),
    .io_full_7                 (io_commit_stall_by_rob),
    .io_stall                  (_dr_reg_io_stall_T),
    .io_inst_valid_wb_0        (_ew_reg1_io_inst_pack_WB_inst_valid),
    .io_inst_valid_wb_1        (_ew_reg2_io_inst_pack_WB_inst_valid),
    .io_inst_valid_wb_2        (_ew_reg3_io_inst_pack_WB_inst_valid),
    .io_inst_valid_wb_3        (_ew_reg4_io_inst_pack_WB_inst_valid),
    .io_rob_index_wb_0         (_ew_reg1_io_inst_pack_WB_rob_index),
    .io_rob_index_wb_1         (_ew_reg2_io_inst_pack_WB_rob_index),
    .io_rob_index_wb_2         (_ew_reg3_io_inst_pack_WB_rob_index),
    .io_rob_index_wb_3         (_ew_reg4_io_inst_pack_WB_rob_index),
    .io_exception_wb_3         (_ew_reg4_io_exception_WB),
    .io_is_ucread_wb_0         (_ew_reg1_io_is_ucread_WB),
    .io_is_ucread_wb_3         (_ew_reg4_io_is_ucread_WB),
    .io_predict_fail_wb_1      (_ew_reg2_io_predict_fail_WB),
    .io_real_jump_wb_1         (_ew_reg2_io_real_jump_WB),
    .io_branch_target_wb_1     (_ew_reg2_io_branch_target_WB),
    .io_branch_target_wb_2     (_ew_reg3_io_csr_wdata_WB),
    .io_branch_target_wb_3     (_ew_reg4_io_vaddr_WB),
    .io_rf_wdata_wb_0          (_ew_reg1_io_alu_out_WB),
    .io_rf_wdata_wb_1          (_ew_reg2_io_alu_out_WB),
    .io_rf_wdata_wb_2          (_ew_reg3_io_md_out_WB),
    .io_rf_wdata_wb_3          (_ew_reg4_io_mem_rdata_WB),
    .io_cmt_en_0               (_rob_io_cmt_en_0),
    .io_cmt_en_1               (_rob_io_cmt_en_1),
    .io_prd_cmt_0              (_rob_io_prd_cmt_0),
    .io_prd_cmt_1              (_rob_io_prd_cmt_1),
    .io_rd_valid_cmt_0         (_rob_io_rd_valid_cmt_0),
    .io_rd_valid_cmt_1         (_rob_io_rd_valid_cmt_1),
    .io_pprd_cmt_0             (_rob_io_pprd_cmt_0),
    .io_pprd_cmt_1             (_rob_io_pprd_cmt_1),
    .io_is_store_num_cmt       (_rob_io_is_store_num_cmt),
    .io_predict_fail_cmt       (_rob_io_predict_fail_cmt),
    .io_pred_update_en_cmt     (_rob_io_pred_update_en_cmt),
    .io_pred_branch_target_cmt (_predict_io_branch_target),
    .io_pred_pc_cmt            (_rob_io_pred_pc_cmt),
    .io_pred_real_jump_cmt     (_predict_io_real_jump),
    .io_pred_br_type_cmt       (_rob_io_pred_br_type_cmt),
    .io_csr_addr_cmt           (_csr_rf_io_waddr),
    .io_csr_wdata_cmt          (_csr_rf_io_wdata),
    .io_csr_we_cmt             (_rob_io_csr_we_cmt),
    .io_rob_index_cmt_0        (_iq4_io_rob_index_cmt),
    .io_rob_index_cmt_1        (_dcache_io_rob_index_CMT),
    .io_eentry_global          (_csr_rf_io_eentry_global),
    .io_tlbreentry_global      (_csr_rf_io_tlbreentry_global),
    .io_badv_cmt               (_csr_rf_io_badv_exp),
    .io_exception_cmt          (_rob_io_exception_cmt),
    .io_is_eret_cmt            (_csr_rf_io_is_eret),
    .io_interrupt_vec          (_rob_io_interrupt_vec),
    .io_tlbwr_en_cmt           (_mmu_io_tlbwr_en),
    .io_tlbrd_en_cmt           (_rob_io_tlbrd_en_cmt),
    .io_tlbfill_en_cmt         (_rob_io_tlbfill_en_cmt),
    .io_tlbsrch_en_cmt         (_csr_rf_io_tlbsrch_en),
    .io_invtlb_en_cmt          (_mmu_io_invtlb_en),
    .io_invtlb_op_cmt          (_mmu_io_invtlb_op),
    .io_invtlb_vaddr_cmt       (_mmu_io_invtlb_vaddr),
    .io_invtlb_asid_cmt        (_mmu_io_invtlb_asid),
    .io_llbit_set_cmt          (_csr_rf_io_llbit_set),
    .io_llbit_clear_cmt        (_csr_rf_io_llbit_clear),
    .io_idle_en_cmt            (_pc_io_is_idle_cmt),
    .io_priv_vec_ex            (_re_reg3_io_inst_pack_EX_priv_vec),
    .io_csr_addr_ex            (_rob_io_csr_addr_ex),
    .io_tlbentry_ex_vppn       (_mmu_io_tlbrd_entry_vppn),
    .io_tlbentry_ex_ps         (_mmu_io_tlbrd_entry_ps),
    .io_tlbentry_ex_g          (_mmu_io_tlbrd_entry_g),
    .io_tlbentry_ex_asid       (_mmu_io_tlbrd_entry_asid),
    .io_tlbentry_ex_e          (_mmu_io_tlbrd_entry_e),
    .io_tlbentry_ex_ppn0       (_mmu_io_tlbrd_entry_ppn0),
    .io_tlbentry_ex_plv0       (_mmu_io_tlbrd_entry_plv0),
    .io_tlbentry_ex_mat0       (_mmu_io_tlbrd_entry_mat0),
    .io_tlbentry_ex_d0         (_mmu_io_tlbrd_entry_d0),
    .io_tlbentry_ex_v0         (_mmu_io_tlbrd_entry_v0),
    .io_tlbentry_ex_ppn1       (_mmu_io_tlbrd_entry_ppn1),
    .io_tlbentry_ex_plv1       (_mmu_io_tlbrd_entry_plv1),
    .io_tlbentry_ex_mat1       (_mmu_io_tlbrd_entry_mat1),
    .io_tlbentry_ex_d1         (_mmu_io_tlbrd_entry_d1),
    .io_tlbentry_ex_v1         (_mmu_io_tlbrd_entry_v1),
    .io_tlbentry_cmt_vppn      (_csr_rf_io_tlbentry_in_vppn),
    .io_tlbentry_cmt_ps        (_csr_rf_io_tlbentry_in_ps),
    .io_tlbentry_cmt_g         (_csr_rf_io_tlbentry_in_g),
    .io_tlbentry_cmt_asid      (_csr_rf_io_tlbentry_in_asid),
    .io_tlbentry_cmt_e         (_csr_rf_io_tlbentry_in_e),
    .io_tlbentry_cmt_ppn0      (_csr_rf_io_tlbentry_in_ppn0),
    .io_tlbentry_cmt_plv0      (_csr_rf_io_tlbentry_in_plv0),
    .io_tlbentry_cmt_mat0      (_csr_rf_io_tlbentry_in_mat0),
    .io_tlbentry_cmt_d0        (_csr_rf_io_tlbentry_in_d0),
    .io_tlbentry_cmt_v0        (_csr_rf_io_tlbentry_in_v0),
    .io_tlbentry_cmt_ppn1      (_csr_rf_io_tlbentry_in_ppn1),
    .io_tlbentry_cmt_plv1      (_csr_rf_io_tlbentry_in_plv1),
    .io_tlbentry_cmt_mat1      (_csr_rf_io_tlbentry_in_mat1),
    .io_tlbentry_cmt_d1        (_csr_rf_io_tlbentry_in_d1),
    .io_tlbentry_cmt_v1        (_csr_rf_io_tlbentry_in_v1),
    .io_invtlb_op_ex           (_rob_io_invtlb_op_ex),
    .io_invtlb_vaddr_ex        (_re_reg3_io_src2_EX),
    .io_invtlb_asid_ex         (_rob_io_invtlb_asid_ex),
    .io_priv_vec_ls            (_re_reg4_io_inst_pack_EX_priv_vec),
    .io_llbit_global           (_csr_rf_io_llbit_global),
    .io_is_ucread_cmt_0        (io_commit_is_ucread_0),
    .io_is_ucread_cmt_1        (io_commit_is_ucread_1),
    .io_rd_cmt_0               (io_commit_rd_0),
    .io_rd_cmt_1               (io_commit_rd_1),
    .io_rf_wdata_cmt_0         (io_commit_rf_wdata_0),
    .io_rf_wdata_cmt_1         (io_commit_rf_wdata_1),
    .io_branch_target_cmt      (_pc_io_branch_target),
    .io_pc_cmt_0               (io_commit_pc_0),
    .io_pc_cmt_1               (io_commit_pc_1),
    .io_csr_diff_addr_cmt_0    (io_commit_csr_waddr_0),
    .io_csr_diff_addr_cmt_1    (io_commit_csr_waddr_1),
    .io_csr_diff_wdata_cmt_0   (io_commit_csr_wdata_0),
    .io_csr_diff_wdata_cmt_1   (io_commit_csr_wdata_1),
    .io_csr_diff_we_cmt_0      (io_commit_csr_we_0),
    .io_csr_diff_we_cmt_1      (io_commit_csr_we_1),
    .io_inst_cmt_0             (io_commit_inst_0),
    .io_inst_cmt_1             (io_commit_inst_1),
    .io_predict_fail_stat_0    (io_commit_predict_fail_0),
    .io_predict_fail_stat_1    (io_commit_predict_fail_1),
    .io_br_type_stat_0         (io_commit_br_type_0),
    .io_br_type_stat_1         (io_commit_br_type_1),
    .io_is_br_stat_0           (io_commit_is_br_0),
    .io_is_br_stat_1           (io_commit_is_br_1)
  );
  Bypass_3 bypass (
    .io_prd_wb_0           (_ew_reg1_io_inst_pack_WB_prd),
    .io_prd_wb_1           (_ew_reg2_io_inst_pack_WB_prd),
    .io_prd_wb_2           (_ew_reg4_io_inst_pack_WB_prd),
    .io_prj_ex_0           (_re_reg1_io_inst_pack_EX_prj),
    .io_prj_ex_1           (_re_reg2_io_inst_pack_EX_prj),
    .io_prj_ex_2           (_ir_reg4_io_inst_pack_EX_prj),
    .io_prk_ex_0           (_re_reg1_io_inst_pack_EX_prk),
    .io_prk_ex_1           (_re_reg2_io_inst_pack_EX_prk),
    .io_prk_ex_2           (_ir_reg4_io_inst_pack_EX_prk),
    .io_prf_wdata_wb_0     (_ew_reg1_io_alu_out_WB),
    .io_prf_wdata_wb_1     (_ew_reg2_io_alu_out_WB),
    .io_prf_wdata_wb_2     (_ew_reg4_io_mem_rdata_WB),
    .io_rd_valid_wb_0      (_ew_reg1_io_inst_pack_WB_rd_valid),
    .io_rd_valid_wb_1      (_ew_reg2_io_inst_pack_WB_rd_valid),
    .io_rd_valid_wb_2      (_ew_reg4_io_inst_pack_WB_rd_valid),
    .io_forward_prj_en_0   (_bypass_io_forward_prj_en_0),
    .io_forward_prj_en_1   (_bypass_io_forward_prj_en_1),
    .io_forward_prj_en_2   (_bypass_io_forward_prj_en_2),
    .io_forward_prk_en_0   (_bypass_io_forward_prk_en_0),
    .io_forward_prk_en_1   (_bypass_io_forward_prk_en_1),
    .io_forward_prk_en_2   (_bypass_io_forward_prk_en_2),
    .io_forward_prj_data_0 (_bypass_io_forward_prj_data_0),
    .io_forward_prj_data_1 (_bypass_io_forward_prj_data_1),
    .io_forward_prj_data_2 (_bypass_io_forward_prj_data_2),
    .io_forward_prk_data_0 (_bypass_io_forward_prk_data_0),
    .io_forward_prk_data_1 (_bypass_io_forward_prk_data_1),
    .io_forward_prk_data_2 (_bypass_io_forward_prk_data_2)
  );
  Arch_Rat arat (
    .clock                 (clock),
    .reset                 (reset),
    .io_cmt_en_0           (_rob_io_cmt_en_0),
    .io_cmt_en_1           (_rob_io_cmt_en_1),
    .io_prd_cmt_0          (_rob_io_prd_cmt_0),
    .io_prd_cmt_1          (_rob_io_prd_cmt_1),
    .io_pprd_cmt_0         (_rob_io_pprd_cmt_0),
    .io_pprd_cmt_1         (_rob_io_pprd_cmt_1),
    .io_rd_valid_cmt_0     (_rob_io_rd_valid_cmt_0),
    .io_rd_valid_cmt_1     (_rob_io_rd_valid_cmt_1),
    .io_arch_rat_0         (_rename_io_arch_rat_0),
    .io_arch_rat_1         (_rename_io_arch_rat_1),
    .io_arch_rat_2         (_rename_io_arch_rat_2),
    .io_arch_rat_3         (_rename_io_arch_rat_3),
    .io_arch_rat_4         (_rename_io_arch_rat_4),
    .io_arch_rat_5         (_rename_io_arch_rat_5),
    .io_arch_rat_6         (_rename_io_arch_rat_6),
    .io_arch_rat_7         (_rename_io_arch_rat_7),
    .io_arch_rat_8         (_rename_io_arch_rat_8),
    .io_arch_rat_9         (_rename_io_arch_rat_9),
    .io_arch_rat_10        (_rename_io_arch_rat_10),
    .io_arch_rat_11        (_rename_io_arch_rat_11),
    .io_arch_rat_12        (_rename_io_arch_rat_12),
    .io_arch_rat_13        (_rename_io_arch_rat_13),
    .io_arch_rat_14        (_rename_io_arch_rat_14),
    .io_arch_rat_15        (_rename_io_arch_rat_15),
    .io_arch_rat_16        (_rename_io_arch_rat_16),
    .io_arch_rat_17        (_rename_io_arch_rat_17),
    .io_arch_rat_18        (_rename_io_arch_rat_18),
    .io_arch_rat_19        (_rename_io_arch_rat_19),
    .io_arch_rat_20        (_rename_io_arch_rat_20),
    .io_arch_rat_21        (_rename_io_arch_rat_21),
    .io_arch_rat_22        (_rename_io_arch_rat_22),
    .io_arch_rat_23        (_rename_io_arch_rat_23),
    .io_arch_rat_24        (_rename_io_arch_rat_24),
    .io_arch_rat_25        (_rename_io_arch_rat_25),
    .io_arch_rat_26        (_rename_io_arch_rat_26),
    .io_arch_rat_27        (_rename_io_arch_rat_27),
    .io_arch_rat_28        (_rename_io_arch_rat_28),
    .io_arch_rat_29        (_rename_io_arch_rat_29),
    .io_arch_rat_30        (_rename_io_arch_rat_30),
    .io_arch_rat_31        (_rename_io_arch_rat_31),
    .io_arch_rat_32        (_rename_io_arch_rat_32),
    .io_arch_rat_33        (_rename_io_arch_rat_33),
    .io_arch_rat_34        (_rename_io_arch_rat_34),
    .io_arch_rat_35        (_rename_io_arch_rat_35),
    .io_arch_rat_36        (_rename_io_arch_rat_36),
    .io_arch_rat_37        (_rename_io_arch_rat_37),
    .io_arch_rat_38        (_rename_io_arch_rat_38),
    .io_arch_rat_39        (_rename_io_arch_rat_39),
    .io_arch_rat_40        (_rename_io_arch_rat_40),
    .io_arch_rat_41        (_rename_io_arch_rat_41),
    .io_arch_rat_42        (_rename_io_arch_rat_42),
    .io_arch_rat_43        (_rename_io_arch_rat_43),
    .io_arch_rat_44        (_rename_io_arch_rat_44),
    .io_arch_rat_45        (_rename_io_arch_rat_45),
    .io_arch_rat_46        (_rename_io_arch_rat_46),
    .io_arch_rat_47        (_rename_io_arch_rat_47),
    .io_arch_rat_48        (_rename_io_arch_rat_48),
    .io_arch_rat_49        (_rename_io_arch_rat_49),
    .io_arch_rat_50        (_rename_io_arch_rat_50),
    .io_arch_rat_51        (_rename_io_arch_rat_51),
    .io_arch_rat_52        (_rename_io_arch_rat_52),
    .io_arch_rat_53        (_rename_io_arch_rat_53),
    .io_arch_rat_54        (_rename_io_arch_rat_54),
    .io_arch_rat_55        (_rename_io_arch_rat_55),
    .io_arch_rat_56        (_rename_io_arch_rat_56),
    .io_arch_rat_57        (_rename_io_arch_rat_57),
    .io_arch_rat_58        (_rename_io_arch_rat_58),
    .io_arch_rat_59        (_rename_io_arch_rat_59),
    .io_arch_rat_60        (_rename_io_arch_rat_60),
    .io_arch_rat_61        (_rename_io_arch_rat_61),
    .io_arch_rat_62        (_rename_io_arch_rat_62),
    .io_arch_rat_63        (_rename_io_arch_rat_63),
    .io_head_arch          (_free_list_io_head_arch),
    .io_top_arch           (_predict_io_top_arch),
    .io_br_type_pred_cmt   (_rob_io_pred_br_type_cmt),
    .io_pc_cmt             (_rob_io_pred_pc_cmt),
    .io_pred_update_en_cmt (_rob_io_pred_update_en_cmt),
    .io_ras_arch_0         (_predict_io_ras_arch_0),
    .io_ras_arch_1         (_predict_io_ras_arch_1),
    .io_ras_arch_2         (_predict_io_ras_arch_2),
    .io_ras_arch_3         (_predict_io_ras_arch_3),
    .io_ras_arch_4         (_predict_io_ras_arch_4),
    .io_ras_arch_5         (_predict_io_ras_arch_5),
    .io_ras_arch_6         (_predict_io_ras_arch_6),
    .io_ras_arch_7         (_predict_io_ras_arch_7)
  );
  assign io_arburst = 2'h1;
  assign io_arid = 4'h0;
  assign io_awburst = 2'h1;
  assign io_awid = 4'h0;
  assign io_commit_en_0 = _rob_io_cmt_en_0;
  assign io_commit_en_1 = _rob_io_cmt_en_1;
  assign io_commit_prd_0 = _rob_io_prd_cmt_0;
  assign io_commit_prd_1 = _rob_io_prd_cmt_1;
  assign io_commit_rd_valid_0 = _rob_io_rd_valid_cmt_0;
  assign io_commit_rd_valid_1 = _rob_io_rd_valid_cmt_1;
  assign io_commit_interrupt =
    _rob_io_exception_cmt[7] & _rob_io_exception_cmt[6:0] == 7'h0;
  assign io_commit_stall_by_fetch_queue = _fq_io_full;
  assign io_commit_stall_by_rename = _free_list_io_empty;
  assign io_commit_stall_by_iq_0 = _iq1_io_full;
  assign io_commit_stall_by_iq_1 = _iq2_io_full;
  assign io_commit_stall_by_iq_2 = _iq3_io_full;
  assign io_commit_stall_by_iq_3 = _iq4_io_full;
  assign io_commit_stall_by_iq_4 = 1'h0;
  assign io_commit_stall_by_sb = _sb_io_full;
  assign io_commit_stall_by_icache = _icache_io_cache_miss_RM;
  assign io_commit_stall_by_dcache = _dcache_io_cache_miss_MEM_4;
  assign io_commit_iq_issue_0 = _sel1_io_inst_issue_valid;
  assign io_commit_iq_issue_1 = _sel2_io_inst_issue_valid;
  assign io_commit_iq_issue_2 = _sel3_io_inst_issue_valid;
  assign io_commit_iq_issue_3 = _sel4_io_inst_issue_valid;
  assign io_commit_iq_issue_4 = 1'h0;
  assign io_commit_tlbfill_en = _rob_io_tlbfill_en_cmt;
  assign io_commit_tlbfill_idx = _stable_cnt_io_value[3:0];
endmodule


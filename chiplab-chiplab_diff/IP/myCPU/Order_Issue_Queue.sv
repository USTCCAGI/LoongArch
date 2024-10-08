// Generated by CIRCT firtool-1.62.0
module Order_Issue_Queue(
  input         clock,
                reset,
                io_insts_disp_valid_0,
                io_insts_disp_valid_1,
  input  [5:0]  io_insts_dispatch_0_prj,
                io_insts_dispatch_0_prk,
  input         io_insts_dispatch_0_rd_valid,
  input  [5:0]  io_insts_dispatch_0_prd,
  input  [31:0] io_insts_dispatch_0_imm,
  input  [4:0]  io_insts_dispatch_0_rob_index,
  input  [9:0]  io_insts_dispatch_0_priv_vec,
  input  [3:0]  io_insts_dispatch_0_alu_op,
  input  [5:0]  io_insts_dispatch_1_prj,
                io_insts_dispatch_1_prk,
  input         io_insts_dispatch_1_rd_valid,
  input  [5:0]  io_insts_dispatch_1_prd,
  input  [31:0] io_insts_dispatch_1_imm,
  input  [4:0]  io_insts_dispatch_1_rob_index,
  input  [9:0]  io_insts_dispatch_1_priv_vec,
  input  [3:0]  io_insts_dispatch_1_alu_op,
  input         io_prj_ready_0,
                io_prj_ready_1,
                io_prk_ready_0,
                io_prk_ready_1,
  input  [5:0]  io_wake_preg_0,
                io_wake_preg_1,
                io_wake_preg_2,
                io_wake_preg_3,
                io_ld_mem_prd,
  input         io_issue_ack,
  output [5:0]  io_insts_issue_inst_prj,
                io_insts_issue_inst_prk,
  output        io_insts_issue_inst_rd_valid,
  output [5:0]  io_insts_issue_inst_prd,
  output [31:0] io_insts_issue_inst_imm,
  output [4:0]  io_insts_issue_inst_rob_index,
  output [9:0]  io_insts_issue_inst_priv_vec,
  output [3:0]  io_insts_issue_inst_alu_op,
  output        io_issue_req,
                io_full,
  input         io_stall,
                io_flush,
                io_dcache_miss
);

  reg  [5:0]  queue_0_inst_prj;
  reg  [5:0]  queue_0_inst_prk;
  reg         queue_0_inst_rd_valid;
  reg  [5:0]  queue_0_inst_prd;
  reg  [31:0] queue_0_inst_imm;
  reg  [4:0]  queue_0_inst_rob_index;
  reg  [9:0]  queue_0_inst_priv_vec;
  reg  [3:0]  queue_0_inst_alu_op;
  reg         queue_0_prj_waked;
  reg         queue_0_prk_waked;
  reg         queue_0_prj_wake_by_ld;
  reg         queue_0_prk_wake_by_ld;
  reg  [5:0]  queue_1_inst_prj;
  reg  [5:0]  queue_1_inst_prk;
  reg         queue_1_inst_rd_valid;
  reg  [5:0]  queue_1_inst_prd;
  reg  [31:0] queue_1_inst_imm;
  reg  [4:0]  queue_1_inst_rob_index;
  reg  [9:0]  queue_1_inst_priv_vec;
  reg  [3:0]  queue_1_inst_alu_op;
  reg         queue_1_prj_waked;
  reg         queue_1_prk_waked;
  reg         queue_1_prj_wake_by_ld;
  reg         queue_1_prk_wake_by_ld;
  reg  [5:0]  queue_2_inst_prj;
  reg  [5:0]  queue_2_inst_prk;
  reg         queue_2_inst_rd_valid;
  reg  [5:0]  queue_2_inst_prd;
  reg  [31:0] queue_2_inst_imm;
  reg  [4:0]  queue_2_inst_rob_index;
  reg  [9:0]  queue_2_inst_priv_vec;
  reg  [3:0]  queue_2_inst_alu_op;
  reg         queue_2_prj_waked;
  reg         queue_2_prk_waked;
  reg         queue_2_prj_wake_by_ld;
  reg         queue_2_prk_wake_by_ld;
  reg  [5:0]  queue_3_inst_prj;
  reg  [5:0]  queue_3_inst_prk;
  reg         queue_3_inst_rd_valid;
  reg  [5:0]  queue_3_inst_prd;
  reg  [31:0] queue_3_inst_imm;
  reg  [4:0]  queue_3_inst_rob_index;
  reg  [9:0]  queue_3_inst_priv_vec;
  reg  [3:0]  queue_3_inst_alu_op;
  reg         queue_3_prj_waked;
  reg         queue_3_prk_waked;
  reg         queue_3_prj_wake_by_ld;
  reg         queue_3_prk_wake_by_ld;
  reg  [2:0]  num;
  reg  [3:0]  qvalid;
  wire [2:0]  _num_pop_T = 3'(num - {2'h0, io_issue_ack});
  wire [3:0]  _qvalid_pop_T_1 = {1'h0, qvalid[3:1]};
  wire [3:0]  qvalid_pop = io_issue_ack ? _qvalid_pop_T_1 : qvalid;
  wire [5:0]  _queue_next_T_inst_prj = io_issue_ack ? queue_1_inst_prj : queue_0_inst_prj;
  wire [5:0]  _queue_next_T_inst_prk = io_issue_ack ? queue_1_inst_prk : queue_0_inst_prk;
  wire        _GEN = ~io_insts_disp_valid_0 & io_insts_disp_valid_1;
  wire        _GEN_0 = _GEN | 1'(1'h0 - _num_pop_T[0]);
  wire [5:0]  _GEN_1 = _GEN_0 ? io_insts_dispatch_1_prj : io_insts_dispatch_0_prj;
  wire [5:0]  queue_next_inst_prj = qvalid_pop[0] ? _queue_next_T_inst_prj : _GEN_1;
  wire [5:0]  _GEN_2 = _GEN_0 ? io_insts_dispatch_1_prk : io_insts_dispatch_0_prk;
  wire [5:0]  queue_next_inst_prk = qvalid_pop[0] ? _queue_next_T_inst_prk : _GEN_2;
  wire [5:0]  _queue_next_T_1_inst_prj =
    io_issue_ack ? queue_2_inst_prj : queue_1_inst_prj;
  wire [5:0]  _queue_next_T_1_inst_prk =
    io_issue_ack ? queue_2_inst_prk : queue_1_inst_prk;
  wire        _GEN_3 = _GEN | 1'(1'h1 - _num_pop_T[0]);
  wire [5:0]  _GEN_4 = _GEN_3 ? io_insts_dispatch_1_prj : io_insts_dispatch_0_prj;
  wire [5:0]  queue_next_1_inst_prj = qvalid_pop[1] ? _queue_next_T_1_inst_prj : _GEN_4;
  wire [5:0]  _GEN_5 = _GEN_3 ? io_insts_dispatch_1_prk : io_insts_dispatch_0_prk;
  wire [5:0]  queue_next_1_inst_prk = qvalid_pop[1] ? _queue_next_T_1_inst_prk : _GEN_5;
  wire [5:0]  _queue_next_T_2_inst_prj =
    io_issue_ack ? queue_3_inst_prj : queue_2_inst_prj;
  wire [5:0]  _queue_next_T_2_inst_prk =
    io_issue_ack ? queue_3_inst_prk : queue_2_inst_prk;
  wire        _GEN_6 = _GEN | 1'(1'h0 - _num_pop_T[0]);
  wire [5:0]  _GEN_7 = _GEN_6 ? io_insts_dispatch_1_prj : io_insts_dispatch_0_prj;
  wire [5:0]  queue_next_2_inst_prj = qvalid_pop[2] ? _queue_next_T_2_inst_prj : _GEN_7;
  wire [5:0]  _GEN_8 = _GEN_6 ? io_insts_dispatch_1_prk : io_insts_dispatch_0_prk;
  wire [5:0]  queue_next_2_inst_prk = qvalid_pop[2] ? _queue_next_T_2_inst_prk : _GEN_8;
  wire        _GEN_9 = _GEN | 1'(1'h1 - _num_pop_T[0]);
  wire [5:0]  _GEN_10 = _GEN_9 ? io_insts_dispatch_1_prj : io_insts_dispatch_0_prj;
  wire [5:0]  queue_next_3_inst_prj = qvalid_pop[3] ? queue_3_inst_prj : _GEN_10;
  wire [5:0]  _GEN_11 = _GEN_9 ? io_insts_dispatch_1_prk : io_insts_dispatch_0_prk;
  wire [5:0]  queue_next_3_inst_prk = qvalid_pop[3] ? queue_3_inst_prk : _GEN_11;
  wire [1:0]  ins_num = 2'({1'h0, io_insts_disp_valid_0} + {1'h0, io_insts_disp_valid_1});
  wire        _GEN_12 = _GEN_0 ? io_prj_ready_1 : io_prj_ready_0;
  wire        _queue_next_T_prj_waked =
    io_issue_ack ? queue_1_prj_waked : queue_0_prj_waked;
  wire        queue_next_prj_waked = qvalid_pop[0] ? _queue_next_T_prj_waked : _GEN_12;
  wire        _GEN_13 = _GEN_0 ? io_prk_ready_1 : io_prk_ready_0;
  wire        _queue_next_T_prk_waked =
    io_issue_ack ? queue_1_prk_waked : queue_0_prk_waked;
  wire        queue_next_prk_waked = qvalid_pop[0] ? _queue_next_T_prk_waked : _GEN_13;
  wire        _GEN_14 =
    _GEN
      ? (queue_next_inst_prj ^ io_ld_mem_prd) == 6'h0 & (|io_ld_mem_prd)
      : (queue_next_inst_prj ^ io_ld_mem_prd) == 6'h0 & (|io_ld_mem_prd);
  wire        _queue_next_T_prj_wake_by_ld =
    io_issue_ack ? queue_1_prj_wake_by_ld : queue_0_prj_wake_by_ld;
  wire        queue_next_prj_wake_by_ld =
    qvalid_pop[0] ? _queue_next_T_prj_wake_by_ld : _GEN_14;
  wire        _GEN_15 =
    _GEN
      ? (queue_next_inst_prk ^ io_ld_mem_prd) == 6'h0 & (|io_ld_mem_prd)
      : (queue_next_inst_prk ^ io_ld_mem_prd) == 6'h0 & (|io_ld_mem_prd);
  wire        _queue_next_T_prk_wake_by_ld =
    io_issue_ack ? queue_1_prk_wake_by_ld : queue_0_prk_wake_by_ld;
  wire        queue_next_prk_wake_by_ld =
    qvalid_pop[0] ? _queue_next_T_prk_wake_by_ld : _GEN_15;
  wire        _GEN_16 = _GEN_3 ? io_prj_ready_1 : io_prj_ready_0;
  wire        _queue_next_T_1_prj_waked =
    io_issue_ack ? queue_2_prj_waked : queue_1_prj_waked;
  wire        queue_next_1_prj_waked =
    qvalid_pop[1] ? _queue_next_T_1_prj_waked : _GEN_16;
  wire        _GEN_17 = _GEN_3 ? io_prk_ready_1 : io_prk_ready_0;
  wire        _queue_next_T_1_prk_waked =
    io_issue_ack ? queue_2_prk_waked : queue_1_prk_waked;
  wire        queue_next_1_prk_waked =
    qvalid_pop[1] ? _queue_next_T_1_prk_waked : _GEN_17;
  wire        _GEN_18 =
    _GEN
      ? (queue_next_1_inst_prj ^ io_ld_mem_prd) == 6'h0 & (|io_ld_mem_prd)
      : (queue_next_1_inst_prj ^ io_ld_mem_prd) == 6'h0 & (|io_ld_mem_prd);
  wire        _queue_next_T_1_prj_wake_by_ld =
    io_issue_ack ? queue_2_prj_wake_by_ld : queue_1_prj_wake_by_ld;
  wire        queue_next_1_prj_wake_by_ld =
    qvalid_pop[1] ? _queue_next_T_1_prj_wake_by_ld : _GEN_18;
  wire        _GEN_19 =
    _GEN
      ? (queue_next_1_inst_prk ^ io_ld_mem_prd) == 6'h0 & (|io_ld_mem_prd)
      : (queue_next_1_inst_prk ^ io_ld_mem_prd) == 6'h0 & (|io_ld_mem_prd);
  wire        _queue_next_T_1_prk_wake_by_ld =
    io_issue_ack ? queue_2_prk_wake_by_ld : queue_1_prk_wake_by_ld;
  wire        queue_next_1_prk_wake_by_ld =
    qvalid_pop[1] ? _queue_next_T_1_prk_wake_by_ld : _GEN_19;
  wire        _GEN_20 = _GEN_6 ? io_prj_ready_1 : io_prj_ready_0;
  wire        _queue_next_T_2_prj_waked =
    io_issue_ack ? queue_3_prj_waked : queue_2_prj_waked;
  wire        queue_next_2_prj_waked =
    qvalid_pop[2] ? _queue_next_T_2_prj_waked : _GEN_20;
  wire        _GEN_21 = _GEN_6 ? io_prk_ready_1 : io_prk_ready_0;
  wire        _queue_next_T_2_prk_waked =
    io_issue_ack ? queue_3_prk_waked : queue_2_prk_waked;
  wire        queue_next_2_prk_waked =
    qvalid_pop[2] ? _queue_next_T_2_prk_waked : _GEN_21;
  wire        _GEN_22 =
    _GEN
      ? (queue_next_2_inst_prj ^ io_ld_mem_prd) == 6'h0 & (|io_ld_mem_prd)
      : (queue_next_2_inst_prj ^ io_ld_mem_prd) == 6'h0 & (|io_ld_mem_prd);
  wire        _queue_next_T_2_prj_wake_by_ld =
    io_issue_ack ? queue_3_prj_wake_by_ld : queue_2_prj_wake_by_ld;
  wire        queue_next_2_prj_wake_by_ld =
    qvalid_pop[2] ? _queue_next_T_2_prj_wake_by_ld : _GEN_22;
  wire        _GEN_23 =
    _GEN
      ? (queue_next_2_inst_prk ^ io_ld_mem_prd) == 6'h0 & (|io_ld_mem_prd)
      : (queue_next_2_inst_prk ^ io_ld_mem_prd) == 6'h0 & (|io_ld_mem_prd);
  wire        _queue_next_T_2_prk_wake_by_ld =
    io_issue_ack ? queue_3_prk_wake_by_ld : queue_2_prk_wake_by_ld;
  wire        queue_next_2_prk_wake_by_ld =
    qvalid_pop[2] ? _queue_next_T_2_prk_wake_by_ld : _GEN_23;
  wire        _GEN_24 = _GEN_9 ? io_prj_ready_1 : io_prj_ready_0;
  wire        queue_next_3_prj_waked = qvalid_pop[3] ? queue_3_prj_waked : _GEN_24;
  wire        _GEN_25 = _GEN_9 ? io_prk_ready_1 : io_prk_ready_0;
  wire        queue_next_3_prk_waked = qvalid_pop[3] ? queue_3_prk_waked : _GEN_25;
  wire        _GEN_26 =
    _GEN
      ? (queue_next_3_inst_prj ^ io_ld_mem_prd) == 6'h0 & (|io_ld_mem_prd)
      : (queue_next_3_inst_prj ^ io_ld_mem_prd) == 6'h0 & (|io_ld_mem_prd);
  wire        queue_next_3_prj_wake_by_ld =
    qvalid_pop[3] ? queue_3_prj_wake_by_ld : _GEN_26;
  wire        _GEN_27 =
    _GEN
      ? (queue_next_3_inst_prk ^ io_ld_mem_prd) == 6'h0 & (|io_ld_mem_prd)
      : (queue_next_3_inst_prk ^ io_ld_mem_prd) == 6'h0 & (|io_ld_mem_prd);
  wire        queue_next_3_prk_wake_by_ld =
    qvalid_pop[3] ? queue_3_prk_wake_by_ld : _GEN_27;
  always @(posedge clock) begin
    if (reset) begin
      queue_0_inst_prj <= 6'h0;
      queue_0_inst_prk <= 6'h0;
      queue_0_inst_rd_valid <= 1'h0;
      queue_0_inst_prd <= 6'h0;
      queue_0_inst_imm <= 32'h0;
      queue_0_inst_rob_index <= 5'h0;
      queue_0_inst_priv_vec <= 10'h0;
      queue_0_inst_alu_op <= 4'h0;
      queue_0_prj_waked <= 1'h0;
      queue_0_prk_waked <= 1'h0;
      queue_0_prj_wake_by_ld <= 1'h0;
      queue_0_prk_wake_by_ld <= 1'h0;
      queue_1_inst_prj <= 6'h0;
      queue_1_inst_prk <= 6'h0;
      queue_1_inst_rd_valid <= 1'h0;
      queue_1_inst_prd <= 6'h0;
      queue_1_inst_imm <= 32'h0;
      queue_1_inst_rob_index <= 5'h0;
      queue_1_inst_priv_vec <= 10'h0;
      queue_1_inst_alu_op <= 4'h0;
      queue_1_prj_waked <= 1'h0;
      queue_1_prk_waked <= 1'h0;
      queue_1_prj_wake_by_ld <= 1'h0;
      queue_1_prk_wake_by_ld <= 1'h0;
      queue_2_inst_prj <= 6'h0;
      queue_2_inst_prk <= 6'h0;
      queue_2_inst_rd_valid <= 1'h0;
      queue_2_inst_prd <= 6'h0;
      queue_2_inst_imm <= 32'h0;
      queue_2_inst_rob_index <= 5'h0;
      queue_2_inst_priv_vec <= 10'h0;
      queue_2_inst_alu_op <= 4'h0;
      queue_2_prj_waked <= 1'h0;
      queue_2_prk_waked <= 1'h0;
      queue_2_prj_wake_by_ld <= 1'h0;
      queue_2_prk_wake_by_ld <= 1'h0;
      queue_3_inst_prj <= 6'h0;
      queue_3_inst_prk <= 6'h0;
      queue_3_inst_rd_valid <= 1'h0;
      queue_3_inst_prd <= 6'h0;
      queue_3_inst_imm <= 32'h0;
      queue_3_inst_rob_index <= 5'h0;
      queue_3_inst_priv_vec <= 10'h0;
      queue_3_inst_alu_op <= 4'h0;
      queue_3_prj_waked <= 1'h0;
      queue_3_prk_waked <= 1'h0;
      queue_3_prj_wake_by_ld <= 1'h0;
      queue_3_prk_wake_by_ld <= 1'h0;
      num <= 3'h0;
      qvalid <= 4'h0;
    end
    else begin
      if (qvalid_pop[0]) begin
        if (io_issue_ack) begin
          queue_0_inst_prj <= queue_1_inst_prj;
          queue_0_inst_prk <= queue_1_inst_prk;
          queue_0_inst_rd_valid <= queue_1_inst_rd_valid;
          queue_0_inst_prd <= queue_1_inst_prd;
          queue_0_inst_imm <= queue_1_inst_imm;
          queue_0_inst_rob_index <= queue_1_inst_rob_index;
          queue_0_inst_priv_vec <= queue_1_inst_priv_vec;
          queue_0_inst_alu_op <= queue_1_inst_alu_op;
        end
      end
      else begin
        queue_0_inst_prj <= _GEN_1;
        queue_0_inst_prk <= _GEN_2;
        queue_0_inst_rd_valid <=
          _GEN_0 ? io_insts_dispatch_1_rd_valid : io_insts_dispatch_0_rd_valid;
        queue_0_inst_prd <= _GEN_0 ? io_insts_dispatch_1_prd : io_insts_dispatch_0_prd;
        queue_0_inst_imm <= _GEN_0 ? io_insts_dispatch_1_imm : io_insts_dispatch_0_imm;
        queue_0_inst_rob_index <=
          _GEN_0 ? io_insts_dispatch_1_rob_index : io_insts_dispatch_0_rob_index;
        queue_0_inst_priv_vec <=
          _GEN_0 ? io_insts_dispatch_1_priv_vec : io_insts_dispatch_0_priv_vec;
        queue_0_inst_alu_op <=
          _GEN_0 ? io_insts_dispatch_1_alu_op : io_insts_dispatch_0_alu_op;
      end
      queue_0_prj_waked <=
        queue_next_prj_waked
        | (|{(queue_next_inst_prj ^ io_wake_preg_3) == 6'h0,
             (queue_next_inst_prj ^ io_wake_preg_2) == 6'h0,
             (queue_next_inst_prj ^ io_wake_preg_1) == 6'h0,
             (queue_next_inst_prj ^ io_wake_preg_0) == 6'h0});
      queue_0_prk_waked <=
        queue_next_prk_waked
        | (|{(queue_next_inst_prk ^ io_wake_preg_3) == 6'h0,
             (queue_next_inst_prk ^ io_wake_preg_2) == 6'h0,
             (queue_next_inst_prk ^ io_wake_preg_1) == 6'h0,
             (queue_next_inst_prk ^ io_wake_preg_0) == 6'h0});
      queue_0_prj_wake_by_ld <=
        (queue_next_inst_prj ^ io_wake_preg_3) == 6'h0 & (|io_wake_preg_3)
        | queue_next_prj_wake_by_ld;
      queue_0_prk_wake_by_ld <=
        (queue_next_inst_prk ^ io_wake_preg_3) == 6'h0 & (|io_wake_preg_3)
        | queue_next_prk_wake_by_ld;
      if (qvalid_pop[1]) begin
        if (io_issue_ack) begin
          queue_1_inst_prj <= queue_2_inst_prj;
          queue_1_inst_prk <= queue_2_inst_prk;
          queue_1_inst_rd_valid <= queue_2_inst_rd_valid;
          queue_1_inst_prd <= queue_2_inst_prd;
          queue_1_inst_imm <= queue_2_inst_imm;
          queue_1_inst_rob_index <= queue_2_inst_rob_index;
          queue_1_inst_priv_vec <= queue_2_inst_priv_vec;
          queue_1_inst_alu_op <= queue_2_inst_alu_op;
        end
      end
      else begin
        queue_1_inst_prj <= _GEN_4;
        queue_1_inst_prk <= _GEN_5;
        queue_1_inst_rd_valid <=
          _GEN_3 ? io_insts_dispatch_1_rd_valid : io_insts_dispatch_0_rd_valid;
        queue_1_inst_prd <= _GEN_3 ? io_insts_dispatch_1_prd : io_insts_dispatch_0_prd;
        queue_1_inst_imm <= _GEN_3 ? io_insts_dispatch_1_imm : io_insts_dispatch_0_imm;
        queue_1_inst_rob_index <=
          _GEN_3 ? io_insts_dispatch_1_rob_index : io_insts_dispatch_0_rob_index;
        queue_1_inst_priv_vec <=
          _GEN_3 ? io_insts_dispatch_1_priv_vec : io_insts_dispatch_0_priv_vec;
        queue_1_inst_alu_op <=
          _GEN_3 ? io_insts_dispatch_1_alu_op : io_insts_dispatch_0_alu_op;
      end
      queue_1_prj_waked <=
        queue_next_1_prj_waked
        | (|{(queue_next_1_inst_prj ^ io_wake_preg_3) == 6'h0,
             (queue_next_1_inst_prj ^ io_wake_preg_2) == 6'h0,
             (queue_next_1_inst_prj ^ io_wake_preg_1) == 6'h0,
             (queue_next_1_inst_prj ^ io_wake_preg_0) == 6'h0});
      queue_1_prk_waked <=
        queue_next_1_prk_waked
        | (|{(queue_next_1_inst_prk ^ io_wake_preg_3) == 6'h0,
             (queue_next_1_inst_prk ^ io_wake_preg_2) == 6'h0,
             (queue_next_1_inst_prk ^ io_wake_preg_1) == 6'h0,
             (queue_next_1_inst_prk ^ io_wake_preg_0) == 6'h0});
      queue_1_prj_wake_by_ld <=
        (queue_next_1_inst_prj ^ io_wake_preg_3) == 6'h0 & (|io_wake_preg_3)
        | queue_next_1_prj_wake_by_ld;
      queue_1_prk_wake_by_ld <=
        (queue_next_1_inst_prk ^ io_wake_preg_3) == 6'h0 & (|io_wake_preg_3)
        | queue_next_1_prk_wake_by_ld;
      if (qvalid_pop[2]) begin
        if (io_issue_ack) begin
          queue_2_inst_prj <= queue_3_inst_prj;
          queue_2_inst_prk <= queue_3_inst_prk;
          queue_2_inst_rd_valid <= queue_3_inst_rd_valid;
          queue_2_inst_prd <= queue_3_inst_prd;
          queue_2_inst_imm <= queue_3_inst_imm;
          queue_2_inst_rob_index <= queue_3_inst_rob_index;
          queue_2_inst_priv_vec <= queue_3_inst_priv_vec;
          queue_2_inst_alu_op <= queue_3_inst_alu_op;
        end
      end
      else begin
        queue_2_inst_prj <= _GEN_7;
        queue_2_inst_prk <= _GEN_8;
        queue_2_inst_rd_valid <=
          _GEN_6 ? io_insts_dispatch_1_rd_valid : io_insts_dispatch_0_rd_valid;
        queue_2_inst_prd <= _GEN_6 ? io_insts_dispatch_1_prd : io_insts_dispatch_0_prd;
        queue_2_inst_imm <= _GEN_6 ? io_insts_dispatch_1_imm : io_insts_dispatch_0_imm;
        queue_2_inst_rob_index <=
          _GEN_6 ? io_insts_dispatch_1_rob_index : io_insts_dispatch_0_rob_index;
        queue_2_inst_priv_vec <=
          _GEN_6 ? io_insts_dispatch_1_priv_vec : io_insts_dispatch_0_priv_vec;
        queue_2_inst_alu_op <=
          _GEN_6 ? io_insts_dispatch_1_alu_op : io_insts_dispatch_0_alu_op;
      end
      queue_2_prj_waked <=
        queue_next_2_prj_waked
        | (|{(queue_next_2_inst_prj ^ io_wake_preg_3) == 6'h0,
             (queue_next_2_inst_prj ^ io_wake_preg_2) == 6'h0,
             (queue_next_2_inst_prj ^ io_wake_preg_1) == 6'h0,
             (queue_next_2_inst_prj ^ io_wake_preg_0) == 6'h0});
      queue_2_prk_waked <=
        queue_next_2_prk_waked
        | (|{(queue_next_2_inst_prk ^ io_wake_preg_3) == 6'h0,
             (queue_next_2_inst_prk ^ io_wake_preg_2) == 6'h0,
             (queue_next_2_inst_prk ^ io_wake_preg_1) == 6'h0,
             (queue_next_2_inst_prk ^ io_wake_preg_0) == 6'h0});
      queue_2_prj_wake_by_ld <=
        (queue_next_2_inst_prj ^ io_wake_preg_3) == 6'h0 & (|io_wake_preg_3)
        | queue_next_2_prj_wake_by_ld;
      queue_2_prk_wake_by_ld <=
        (queue_next_2_inst_prk ^ io_wake_preg_3) == 6'h0 & (|io_wake_preg_3)
        | queue_next_2_prk_wake_by_ld;
      if (qvalid_pop[3]) begin
      end
      else begin
        queue_3_inst_prj <= _GEN_10;
        queue_3_inst_prk <= _GEN_11;
        queue_3_inst_rd_valid <=
          _GEN_9 ? io_insts_dispatch_1_rd_valid : io_insts_dispatch_0_rd_valid;
        queue_3_inst_prd <= _GEN_9 ? io_insts_dispatch_1_prd : io_insts_dispatch_0_prd;
        queue_3_inst_imm <= _GEN_9 ? io_insts_dispatch_1_imm : io_insts_dispatch_0_imm;
        queue_3_inst_rob_index <=
          _GEN_9 ? io_insts_dispatch_1_rob_index : io_insts_dispatch_0_rob_index;
        queue_3_inst_priv_vec <=
          _GEN_9 ? io_insts_dispatch_1_priv_vec : io_insts_dispatch_0_priv_vec;
        queue_3_inst_alu_op <=
          _GEN_9 ? io_insts_dispatch_1_alu_op : io_insts_dispatch_0_alu_op;
      end
      queue_3_prj_waked <=
        queue_next_3_prj_waked
        | (|{(queue_next_3_inst_prj ^ io_wake_preg_3) == 6'h0,
             (queue_next_3_inst_prj ^ io_wake_preg_2) == 6'h0,
             (queue_next_3_inst_prj ^ io_wake_preg_1) == 6'h0,
             (queue_next_3_inst_prj ^ io_wake_preg_0) == 6'h0});
      queue_3_prk_waked <=
        queue_next_3_prk_waked
        | (|{(queue_next_3_inst_prk ^ io_wake_preg_3) == 6'h0,
             (queue_next_3_inst_prk ^ io_wake_preg_2) == 6'h0,
             (queue_next_3_inst_prk ^ io_wake_preg_1) == 6'h0,
             (queue_next_3_inst_prk ^ io_wake_preg_0) == 6'h0});
      queue_3_prj_wake_by_ld <=
        (queue_next_3_inst_prj ^ io_wake_preg_3) == 6'h0 & (|io_wake_preg_3)
        | queue_next_3_prj_wake_by_ld;
      queue_3_prk_wake_by_ld <=
        (queue_next_3_inst_prk ^ io_wake_preg_3) == 6'h0 & (|io_wake_preg_3)
        | queue_next_3_prk_wake_by_ld;
      if (io_flush) begin
        num <= 3'h0;
        qvalid <= 4'h0;
      end
      else begin
        if (io_stall | qvalid[2])
          num <= _num_pop_T;
        else
          num <= 3'(_num_pop_T + {1'h0, ins_num});
        if (io_stall) begin
          if (io_issue_ack)
            qvalid <= _qvalid_pop_T_1;
        end
        else if (ins_num == 2'h2)
          qvalid <= {qvalid_pop[1:0], 2'h3};
        else if (ins_num == 2'h1)
          qvalid <= {qvalid_pop[2:0], 1'h1};
        else if (io_issue_ack)
          qvalid <= _qvalid_pop_T_1;
      end
    end
  end // always @(posedge)
  assign io_insts_issue_inst_prj = queue_0_inst_prj;
  assign io_insts_issue_inst_prk = queue_0_inst_prk;
  assign io_insts_issue_inst_rd_valid = queue_0_inst_rd_valid;
  assign io_insts_issue_inst_prd = queue_0_inst_prd;
  assign io_insts_issue_inst_imm = queue_0_inst_imm;
  assign io_insts_issue_inst_rob_index = queue_0_inst_rob_index;
  assign io_insts_issue_inst_priv_vec = queue_0_inst_priv_vec;
  assign io_insts_issue_inst_alu_op = queue_0_inst_alu_op;
  assign io_issue_req =
    qvalid[0] & queue_0_prj_waked & queue_0_prk_waked
    & ~((queue_0_prj_wake_by_ld & (queue_0_inst_prj ^ io_ld_mem_prd) == 6'h0
         | queue_0_prk_wake_by_ld & (queue_0_inst_prk ^ io_ld_mem_prd) == 6'h0)
        & io_dcache_miss);
  assign io_full = qvalid[2];
endmodule


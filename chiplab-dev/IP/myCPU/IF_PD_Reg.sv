// Generated by CIRCT firtool-1.62.0
module IF_PD_Reg(
  input         clock,
                reset,
                io_flush,
                io_stall,
  input  [31:0] io_insts_pack_IF_0_pc,
  input         io_insts_pack_IF_0_inst_valid,
                io_insts_pack_IF_0_predict_jump,
  input  [31:0] io_insts_pack_IF_0_pred_npc,
  input  [7:0]  io_insts_pack_IF_0_exception,
  input  [31:0] io_insts_pack_IF_0_inst,
                io_insts_pack_IF_1_pc,
  input         io_insts_pack_IF_1_inst_valid,
                io_insts_pack_IF_1_predict_jump,
  input  [31:0] io_insts_pack_IF_1_pred_npc,
  input  [7:0]  io_insts_pack_IF_1_exception,
  input  [31:0] io_insts_pack_IF_1_inst,
                io_npc4_IF_0,
                io_npc4_IF_1,
  output [31:0] io_insts_pack_PD_0_pc,
  output        io_insts_pack_PD_0_inst_valid,
                io_insts_pack_PD_0_predict_jump,
  output [31:0] io_insts_pack_PD_0_pred_npc,
  output [7:0]  io_insts_pack_PD_0_exception,
  output [31:0] io_insts_pack_PD_0_inst,
                io_insts_pack_PD_1_pc,
  output        io_insts_pack_PD_1_inst_valid,
                io_insts_pack_PD_1_predict_jump,
  output [31:0] io_insts_pack_PD_1_pred_npc,
  output [7:0]  io_insts_pack_PD_1_exception,
  output [31:0] io_insts_pack_PD_1_inst,
                io_npc4_PD_0,
                io_npc4_PD_1
);

  reg  [31:0] insts_pack_reg_0_pc;
  reg         insts_pack_reg_0_inst_valid;
  reg         insts_pack_reg_0_predict_jump;
  reg  [31:0] insts_pack_reg_0_pred_npc;
  reg  [7:0]  insts_pack_reg_0_exception;
  reg  [31:0] insts_pack_reg_0_inst;
  reg  [31:0] insts_pack_reg_1_pc;
  reg         insts_pack_reg_1_inst_valid;
  reg         insts_pack_reg_1_predict_jump;
  reg  [31:0] insts_pack_reg_1_pred_npc;
  reg  [7:0]  insts_pack_reg_1_exception;
  reg  [31:0] insts_pack_reg_1_inst;
  reg  [31:0] npc4_reg_0;
  reg  [31:0] npc4_reg_1;
  wire        _GEN =
    io_stall ? insts_pack_reg_0_inst_valid : io_insts_pack_IF_0_inst_valid;
  wire        _GEN_0 =
    io_stall ? insts_pack_reg_0_predict_jump : io_insts_pack_IF_0_predict_jump;
  wire        _GEN_1 =
    io_stall ? insts_pack_reg_1_inst_valid : io_insts_pack_IF_1_inst_valid;
  wire        _GEN_2 =
    io_stall ? insts_pack_reg_1_predict_jump : io_insts_pack_IF_1_predict_jump;
  always @(posedge clock) begin
    if (reset) begin
      insts_pack_reg_0_pc <= 32'h0;
      insts_pack_reg_0_inst_valid <= 1'h0;
      insts_pack_reg_0_predict_jump <= 1'h0;
      insts_pack_reg_0_pred_npc <= 32'h0;
      insts_pack_reg_0_exception <= 8'h0;
      insts_pack_reg_0_inst <= 32'h0;
      insts_pack_reg_1_pc <= 32'h0;
      insts_pack_reg_1_inst_valid <= 1'h0;
      insts_pack_reg_1_predict_jump <= 1'h0;
      insts_pack_reg_1_pred_npc <= 32'h0;
      insts_pack_reg_1_exception <= 8'h0;
      insts_pack_reg_1_inst <= 32'h0;
      npc4_reg_0 <= 32'h0;
      npc4_reg_1 <= 32'h0;
    end
    else begin
      if (io_flush) begin
        insts_pack_reg_0_pc <= 32'h0;
        insts_pack_reg_0_pred_npc <= 32'h0;
        insts_pack_reg_0_exception <= 8'h0;
        insts_pack_reg_0_inst <= 32'h0;
        insts_pack_reg_1_pc <= 32'h0;
        insts_pack_reg_1_pred_npc <= 32'h0;
        insts_pack_reg_1_exception <= 8'h0;
        insts_pack_reg_1_inst <= 32'h0;
      end
      else if (io_stall) begin
      end
      else begin
        insts_pack_reg_0_pc <= io_insts_pack_IF_0_pc;
        insts_pack_reg_0_pred_npc <= io_insts_pack_IF_0_pred_npc;
        insts_pack_reg_0_exception <= io_insts_pack_IF_0_exception;
        insts_pack_reg_0_inst <= io_insts_pack_IF_0_inst;
        insts_pack_reg_1_pc <= io_insts_pack_IF_1_pc;
        insts_pack_reg_1_pred_npc <= io_insts_pack_IF_1_pred_npc;
        insts_pack_reg_1_exception <= io_insts_pack_IF_1_exception;
        insts_pack_reg_1_inst <= io_insts_pack_IF_1_inst;
      end
      insts_pack_reg_0_inst_valid <= ~io_flush & _GEN;
      insts_pack_reg_0_predict_jump <= ~io_flush & _GEN_0;
      insts_pack_reg_1_inst_valid <= ~io_flush & _GEN_1;
      insts_pack_reg_1_predict_jump <= ~io_flush & _GEN_2;
      if (io_flush | io_stall) begin
      end
      else begin
        npc4_reg_0 <= io_npc4_IF_0;
        npc4_reg_1 <= io_npc4_IF_1;
      end
    end
  end // always @(posedge)
  assign io_insts_pack_PD_0_pc = insts_pack_reg_0_pc;
  assign io_insts_pack_PD_0_inst_valid = insts_pack_reg_0_inst_valid;
  assign io_insts_pack_PD_0_predict_jump = insts_pack_reg_0_predict_jump;
  assign io_insts_pack_PD_0_pred_npc = insts_pack_reg_0_pred_npc;
  assign io_insts_pack_PD_0_exception = insts_pack_reg_0_exception;
  assign io_insts_pack_PD_0_inst = insts_pack_reg_0_inst;
  assign io_insts_pack_PD_1_pc = insts_pack_reg_1_pc;
  assign io_insts_pack_PD_1_inst_valid = insts_pack_reg_1_inst_valid;
  assign io_insts_pack_PD_1_predict_jump = insts_pack_reg_1_predict_jump;
  assign io_insts_pack_PD_1_pred_npc = insts_pack_reg_1_pred_npc;
  assign io_insts_pack_PD_1_exception = insts_pack_reg_1_exception;
  assign io_insts_pack_PD_1_inst = insts_pack_reg_1_inst;
  assign io_npc4_PD_0 = npc4_reg_0;
  assign io_npc4_PD_1 = npc4_reg_1;
endmodule


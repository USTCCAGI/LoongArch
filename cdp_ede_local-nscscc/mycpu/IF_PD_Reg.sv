// Generated by CIRCT firtool-1.62.0
module IF_PD_Reg(
  input         clock,
                reset,
                io_flush,
                io_stall,
  input  [31:0] io_insts_pack_IF_pc_0,
                io_insts_pack_IF_pc_1,
  input         io_insts_pack_IF_inst_valid_0,
                io_insts_pack_IF_inst_valid_1,
                io_insts_pack_IF_predict_jump_0,
                io_insts_pack_IF_predict_jump_1,
  input  [31:0] io_insts_pack_IF_pred_npc_0,
                io_insts_pack_IF_pred_npc_1,
  input  [7:0]  io_insts_pack_IF_exception_0,
                io_insts_pack_IF_exception_1,
  input  [31:0] io_insts_pack_IF_inst_0,
                io_insts_pack_IF_inst_1,
                io_npc4_IF_0,
                io_npc4_IF_1,
  output [31:0] io_insts_pack_PD_pc_0,
                io_insts_pack_PD_pc_1,
  output        io_insts_pack_PD_inst_valid_0,
                io_insts_pack_PD_inst_valid_1,
                io_insts_pack_PD_predict_jump_0,
                io_insts_pack_PD_predict_jump_1,
  output [31:0] io_insts_pack_PD_pred_npc_0,
                io_insts_pack_PD_pred_npc_1,
  output [7:0]  io_insts_pack_PD_exception_0,
                io_insts_pack_PD_exception_1,
  output [31:0] io_insts_pack_PD_inst_0,
                io_insts_pack_PD_inst_1,
                io_npc4_PD_0,
                io_npc4_PD_1
);

  reg  [31:0] insts_pack_reg_pc_0;
  reg  [31:0] insts_pack_reg_pc_1;
  reg         insts_pack_reg_inst_valid_0;
  reg         insts_pack_reg_inst_valid_1;
  reg         insts_pack_reg_predict_jump_0;
  reg         insts_pack_reg_predict_jump_1;
  reg  [31:0] insts_pack_reg_pred_npc_0;
  reg  [31:0] insts_pack_reg_pred_npc_1;
  reg  [7:0]  insts_pack_reg_exception_0;
  reg  [7:0]  insts_pack_reg_exception_1;
  reg  [31:0] insts_pack_reg_inst_0;
  reg  [31:0] insts_pack_reg_inst_1;
  reg  [31:0] npc4_reg_0;
  reg  [31:0] npc4_reg_1;
  wire        _GEN =
    io_stall ? insts_pack_reg_inst_valid_0 : io_insts_pack_IF_inst_valid_0;
  wire        _GEN_0 =
    io_stall ? insts_pack_reg_inst_valid_1 : io_insts_pack_IF_inst_valid_1;
  wire        _GEN_1 =
    io_stall ? insts_pack_reg_predict_jump_0 : io_insts_pack_IF_predict_jump_0;
  wire        _GEN_2 =
    io_stall ? insts_pack_reg_predict_jump_1 : io_insts_pack_IF_predict_jump_1;
  always @(posedge clock) begin
    if (reset) begin
      insts_pack_reg_pc_0 <= 32'h0;
      insts_pack_reg_pc_1 <= 32'h0;
      insts_pack_reg_inst_valid_0 <= 1'h0;
      insts_pack_reg_inst_valid_1 <= 1'h0;
      insts_pack_reg_predict_jump_0 <= 1'h0;
      insts_pack_reg_predict_jump_1 <= 1'h0;
      insts_pack_reg_pred_npc_0 <= 32'h0;
      insts_pack_reg_pred_npc_1 <= 32'h0;
      insts_pack_reg_exception_0 <= 8'h0;
      insts_pack_reg_exception_1 <= 8'h0;
      insts_pack_reg_inst_0 <= 32'h0;
      insts_pack_reg_inst_1 <= 32'h0;
      npc4_reg_0 <= 32'h0;
      npc4_reg_1 <= 32'h0;
    end
    else begin
      if (io_flush) begin
        insts_pack_reg_pc_0 <= 32'h0;
        insts_pack_reg_pc_1 <= 32'h0;
        insts_pack_reg_pred_npc_0 <= 32'h0;
        insts_pack_reg_pred_npc_1 <= 32'h0;
        insts_pack_reg_exception_0 <= 8'h0;
        insts_pack_reg_exception_1 <= 8'h0;
        insts_pack_reg_inst_0 <= 32'h0;
        insts_pack_reg_inst_1 <= 32'h0;
      end
      else if (io_stall) begin
      end
      else begin
        insts_pack_reg_pc_0 <= io_insts_pack_IF_pc_0;
        insts_pack_reg_pc_1 <= io_insts_pack_IF_pc_1;
        insts_pack_reg_pred_npc_0 <= io_insts_pack_IF_pred_npc_0;
        insts_pack_reg_pred_npc_1 <= io_insts_pack_IF_pred_npc_1;
        insts_pack_reg_exception_0 <= io_insts_pack_IF_exception_0;
        insts_pack_reg_exception_1 <= io_insts_pack_IF_exception_1;
        insts_pack_reg_inst_0 <= io_insts_pack_IF_inst_0;
        insts_pack_reg_inst_1 <= io_insts_pack_IF_inst_1;
      end
      insts_pack_reg_inst_valid_0 <= ~io_flush & _GEN;
      insts_pack_reg_inst_valid_1 <= ~io_flush & _GEN_0;
      insts_pack_reg_predict_jump_0 <= ~io_flush & _GEN_1;
      insts_pack_reg_predict_jump_1 <= ~io_flush & _GEN_2;
      if (io_flush | io_stall) begin
      end
      else begin
        npc4_reg_0 <= io_npc4_IF_0;
        npc4_reg_1 <= io_npc4_IF_1;
      end
    end
  end // always @(posedge)
  assign io_insts_pack_PD_pc_0 = insts_pack_reg_pc_0;
  assign io_insts_pack_PD_pc_1 = insts_pack_reg_pc_1;
  assign io_insts_pack_PD_inst_valid_0 = insts_pack_reg_inst_valid_0;
  assign io_insts_pack_PD_inst_valid_1 = insts_pack_reg_inst_valid_1;
  assign io_insts_pack_PD_predict_jump_0 = insts_pack_reg_predict_jump_0;
  assign io_insts_pack_PD_predict_jump_1 = insts_pack_reg_predict_jump_1;
  assign io_insts_pack_PD_pred_npc_0 = insts_pack_reg_pred_npc_0;
  assign io_insts_pack_PD_pred_npc_1 = insts_pack_reg_pred_npc_1;
  assign io_insts_pack_PD_exception_0 = insts_pack_reg_exception_0;
  assign io_insts_pack_PD_exception_1 = insts_pack_reg_exception_1;
  assign io_insts_pack_PD_inst_0 = insts_pack_reg_inst_0;
  assign io_insts_pack_PD_inst_1 = insts_pack_reg_inst_1;
  assign io_npc4_PD_0 = npc4_reg_0;
  assign io_npc4_PD_1 = npc4_reg_1;
endmodule


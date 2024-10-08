// Generated by CIRCT firtool-1.62.0
module Order_Select_1(
  input  [5:0]  io_insts_issue_inst_prj,
                io_insts_issue_inst_prk,
  input         io_insts_issue_inst_rd_valid,
  input  [5:0]  io_insts_issue_inst_prd,
  input  [31:0] io_insts_issue_inst_imm,
  input  [4:0]  io_insts_issue_inst_rob_index,
                io_insts_issue_inst_mem_type,
  input  [2:0]  io_insts_issue_inst_priv_vec,
  input         io_issue_req,
                io_stall,
  output        io_issue_ack,
  output [5:0]  io_inst_issue_inst_prj,
                io_inst_issue_inst_prk,
  output        io_inst_issue_inst_rd_valid,
  output [5:0]  io_inst_issue_inst_prd,
  output [31:0] io_inst_issue_inst_imm,
  output [4:0]  io_inst_issue_inst_rob_index,
                io_inst_issue_inst_mem_type,
  output [2:0]  io_inst_issue_inst_priv_vec,
  output        io_inst_issue_valid
);

  wire choice = io_issue_req & ~io_stall;
  assign io_issue_ack = choice;
  assign io_inst_issue_inst_prj = choice ? io_insts_issue_inst_prj : 6'h0;
  assign io_inst_issue_inst_prk = choice ? io_insts_issue_inst_prk : 6'h0;
  assign io_inst_issue_inst_rd_valid = choice & io_insts_issue_inst_rd_valid;
  assign io_inst_issue_inst_prd = choice ? io_insts_issue_inst_prd : 6'h0;
  assign io_inst_issue_inst_imm = choice ? io_insts_issue_inst_imm : 32'h0;
  assign io_inst_issue_inst_rob_index = choice ? io_insts_issue_inst_rob_index : 5'h0;
  assign io_inst_issue_inst_mem_type = choice ? io_insts_issue_inst_mem_type : 5'h0;
  assign io_inst_issue_inst_priv_vec = choice ? io_insts_issue_inst_priv_vec : 3'h0;
  assign io_inst_issue_valid = choice;
endmodule


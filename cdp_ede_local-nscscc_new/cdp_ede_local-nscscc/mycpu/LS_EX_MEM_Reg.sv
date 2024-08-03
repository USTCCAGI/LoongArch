// Generated by CIRCT firtool-1.62.0
module LS_EX_MEM_Reg(
  input         clock,
                reset,
                io_flush,
                io_stall,
                io_inst_pack_EX_rd_valid,
  input  [5:0]  io_inst_pack_EX_prd,
  input  [4:0]  io_inst_pack_EX_rob_index,
                io_inst_pack_EX_mem_type,
  input  [2:0]  io_inst_pack_EX_priv_vec,
  input         io_inst_pack_EX_inst_valid,
                io_is_ucread_EX,
  input  [31:0] io_src1_EX,
  input  [63:0] io_prd_EX_2,
  input  [7:0]  io_exception_EX,
  output        io_inst_pack_MEM_rd_valid,
  output [5:0]  io_inst_pack_MEM_prd,
  output [4:0]  io_inst_pack_MEM_rob_index,
                io_inst_pack_MEM_mem_type,
  output [2:0]  io_inst_pack_MEM_priv_vec,
  output        io_inst_pack_MEM_inst_valid,
                io_is_ucread_MEM,
  output [31:0] io_src1_MEM,
  output        io_llbit_MEM,
  output [63:0] io_prd_MEM_2,
  output [7:0]  io_exception_MEM
);

  reg         inst_pack_reg_rd_valid;
  reg  [5:0]  inst_pack_reg_prd;
  reg  [4:0]  inst_pack_reg_rob_index;
  reg  [4:0]  inst_pack_reg_mem_type;
  reg  [2:0]  inst_pack_reg_priv_vec;
  reg         inst_pack_reg_inst_valid;
  reg         is_ucread_Reg;
  reg  [31:0] src1_reg;
  reg         llbit_reg;
  reg  [63:0] prd_reg_2;
  reg  [7:0]  exception_reg;
  wire        _GEN = io_flush | io_stall;
  wire        _GEN_0 = io_stall ? inst_pack_reg_rd_valid : io_inst_pack_EX_rd_valid;
  wire        _GEN_1 = io_stall ? inst_pack_reg_inst_valid : io_inst_pack_EX_inst_valid;
  always @(posedge clock) begin
    if (reset) begin
      inst_pack_reg_rd_valid <= 1'h0;
      inst_pack_reg_prd <= 6'h0;
      inst_pack_reg_rob_index <= 5'h0;
      inst_pack_reg_mem_type <= 5'h0;
      inst_pack_reg_priv_vec <= 3'h0;
      inst_pack_reg_inst_valid <= 1'h0;
      is_ucread_Reg <= 1'h0;
      src1_reg <= 32'h0;
      llbit_reg <= 1'h0;
      prd_reg_2 <= 64'h0;
      exception_reg <= 8'h0;
    end
    else begin
      inst_pack_reg_rd_valid <= ~io_flush & _GEN_0;
      if (io_flush) begin
        inst_pack_reg_prd <= 6'h0;
        inst_pack_reg_rob_index <= 5'h0;
        inst_pack_reg_mem_type <= 5'h0;
        inst_pack_reg_priv_vec <= 3'h0;
        prd_reg_2 <= 64'h0;
      end
      else if (io_stall) begin
      end
      else begin
        inst_pack_reg_prd <= io_inst_pack_EX_prd;
        inst_pack_reg_rob_index <= io_inst_pack_EX_rob_index;
        inst_pack_reg_mem_type <= io_inst_pack_EX_mem_type;
        inst_pack_reg_priv_vec <= io_inst_pack_EX_priv_vec;
        prd_reg_2 <= io_prd_EX_2;
      end
      inst_pack_reg_inst_valid <= ~io_flush & _GEN_1;
      if (_GEN) begin
      end
      else begin
        is_ucread_Reg <= io_is_ucread_EX;
        src1_reg <= io_src1_EX;
      end
      llbit_reg <= _GEN & llbit_reg;
      if (_GEN) begin
      end
      else
        exception_reg <= io_exception_EX;
    end
  end // always @(posedge)
  assign io_inst_pack_MEM_rd_valid = inst_pack_reg_rd_valid;
  assign io_inst_pack_MEM_prd = inst_pack_reg_prd;
  assign io_inst_pack_MEM_rob_index = inst_pack_reg_rob_index;
  assign io_inst_pack_MEM_mem_type = inst_pack_reg_mem_type;
  assign io_inst_pack_MEM_priv_vec = inst_pack_reg_priv_vec;
  assign io_inst_pack_MEM_inst_valid = inst_pack_reg_inst_valid;
  assign io_is_ucread_MEM = is_ucread_Reg;
  assign io_src1_MEM = src1_reg;
  assign io_llbit_MEM = llbit_reg;
  assign io_prd_MEM_2 = prd_reg_2;
  assign io_exception_MEM = exception_reg;
endmodule


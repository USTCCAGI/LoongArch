// Generated by CIRCT firtool-1.62.0
module Exception_LS(
  input  [31:0] io_addr_EX,
  input  [4:0]  io_mem_type_EX,
  output [7:0]  io_exception_ls
);

  wire [3:0][3:0] _GEN = '{4'h7, 4'h3, 4'h1, 4'h0};
  assign io_exception_ls =
    (|(io_addr_EX[3:0] & _GEN[io_mem_type_EX[1:0]])) ? 8'h89 : 8'h0;
endmodule


// Generated by CIRCT firtool-1.62.0
module CSR_Regfile(
  input         clock,
                reset,
  input  [13:0] io_raddr,
  output [31:0] io_rdata,
  input  [13:0] io_waddr,
  input         io_we,
  input  [31:0] io_wdata,
  input  [7:0]  io_exception,
  input  [31:0] io_badv_exp,
  input         io_is_eret,
  input  [31:0] io_pc_exp,
  output [31:0] io_eentry_global,
  output [11:0] io_interrupt_vec,
  output [5:0]  io_crmd_trans,
  output [12:0] io_estat_13
);

  reg  [31:0] crmd;
  reg  [31:0] prmd;
  reg  [31:0] euen;
  reg  [31:0] ecfg;
  reg  [31:0] estat;
  reg         timer_reg;
  reg  [31:0] era;
  reg  [31:0] badv;
  reg  [31:0] eentry;
  reg  [31:0] coreid;
  reg  [31:0] data0;
  reg  [31:0] data1;
  reg  [31:0] data2;
  reg  [31:0] data3;
  reg  [31:0] pgdl;
  reg  [31:0] pgdh;
  reg  [31:0] pgd;
  reg  [31:0] dmw0;
  reg  [31:0] dmw1;
  reg  [31:0] tid;
  reg  [31:0] tcfg;
  reg  [31:0] tval;
  reg  [31:0] tval_edge;
  wire [31:0] _GEN = io_raddr == 14'h42 ? tval : 32'h0;
  wire [31:0] _GEN_0 = io_raddr == 14'h41 ? tcfg : _GEN;
  wire [31:0] _GEN_1 = io_raddr == 14'h40 ? tid : _GEN_0;
  wire [31:0] _GEN_2 = io_raddr == 14'h181 ? dmw1 : _GEN_1;
  wire [31:0] _GEN_3 = io_raddr == 14'h180 ? dmw0 : _GEN_2;
  wire [31:0] _GEN_4 = io_raddr == 14'h88 ? 32'h0 : _GEN_3;
  wire [31:0] _GEN_5 = io_raddr == 14'h1B ? pgd : _GEN_4;
  wire [31:0] _GEN_6 = io_raddr == 14'h1A ? pgdh : _GEN_5;
  wire [31:0] _GEN_7 = io_raddr == 14'h19 ? pgdl : _GEN_6;
  wire [31:0] _GEN_8 =
    io_raddr == 14'h60 | io_raddr == 14'h10 | io_raddr == 14'h11 | io_raddr == 14'h12
    | io_raddr == 14'h13 | io_raddr == 14'h18
      ? 32'h0
      : _GEN_7;
  wire [31:0] _GEN_9 = io_raddr == 14'h33 ? data3 : _GEN_8;
  wire [31:0] _GEN_10 = io_raddr == 14'h32 ? data2 : _GEN_9;
  wire [31:0] _GEN_11 = io_raddr == 14'h31 ? data1 : _GEN_10;
  wire [31:0] _GEN_12 = io_raddr == 14'h30 ? data0 : _GEN_11;
  wire [31:0] _GEN_13 = io_raddr == 14'h20 ? coreid : _GEN_12;
  wire [31:0] _GEN_14 = io_raddr == 14'hC ? eentry : _GEN_13;
  wire [31:0] _GEN_15 = io_raddr == 14'h7 ? badv : _GEN_14;
  wire [31:0] _GEN_16 = io_raddr == 14'h6 ? era : _GEN_15;
  wire [31:0] _GEN_17 = io_raddr == 14'h5 ? estat : _GEN_16;
  wire [31:0] _GEN_18 = io_raddr == 14'h4 ? ecfg : _GEN_17;
  wire [31:0] _GEN_19 = io_raddr == 14'h2 ? euen : _GEN_18;
  wire [31:0] _GEN_20 = io_raddr == 14'h1 ? prmd : _GEN_19;
  wire [30:0] _GEN_21 =
    io_we & io_waddr == 14'h5
      ? {estat[30:16], 4'h0, timer_reg, 9'h0, io_wdata[1:0]}
      : {estat[30:16], 4'h0, timer_reg, 9'h0, estat[1:0]};
  wire [29:0] _tval_T_6 = tcfg[1] ? {tcfg[29:2], 2'h1} : 30'h0;
  wire        _GEN_22 = tval == 32'h0;
  always @(posedge clock) begin
    if (reset) begin
      crmd <= 32'h8;
      prmd <= 32'h0;
      euen <= 32'h0;
      ecfg <= 32'h0;
      estat <= 32'h0;
      timer_reg <= 1'h0;
      era <= 32'h0;
      badv <= 32'h0;
      eentry <= 32'h0;
      coreid <= 32'h0;
      data0 <= 32'h0;
      data1 <= 32'h0;
      data2 <= 32'h0;
      data3 <= 32'h0;
      pgdl <= 32'h0;
      pgdh <= 32'h0;
      pgd <= 32'h0;
      dmw0 <= 32'h0;
      dmw1 <= 32'h0;
      tid <= 32'h0;
      tcfg <= 32'h0;
      tval <= 32'h0;
    end
    else begin
      if (io_exception[7]) begin
        crmd <= {crmd[31:3], 3'h0};
        prmd <= {prmd[31:3], crmd[2:0]};
        estat <= {9'h0, io_exception[6:0], estat[15:0]};
        era <= io_pc_exp;
      end
      else begin
        if (io_is_eret)
          crmd <= {crmd[31:3], prmd[2:0]};
        else if (io_we & io_waddr == 14'h0)
          crmd <= {23'h0, io_wdata[8:0]};
        if (io_we & io_waddr == 14'h1)
          prmd <= {29'h0, io_wdata[2:0]};
        estat <= {1'h0, _GEN_21};
        if (io_we & io_waddr == 14'h6)
          era <= io_wdata;
      end
      if (io_we & io_waddr == 14'h2)
        euen <= {31'h0, io_wdata[0]};
      if (io_we & io_waddr == 14'h4)
        ecfg <= {19'h0, io_wdata[12:11], 1'h0, io_wdata[9:0]};
      timer_reg <=
        ~(io_we & io_waddr == 14'h44 & io_wdata[0])
        & (tcfg[0] & _GEN_22 & tval_edge == 32'h1 | timer_reg);
      if (io_exception[7] & (io_exception[5:0] == 6'h8 | io_exception[6:0] == 7'h9))
        badv <= io_badv_exp;
      else if (io_we & io_waddr == 14'h7)
        badv <= io_wdata;
      if (io_we & io_waddr == 14'hC)
        eentry <= {io_wdata[31:6], 6'h0};
      if (io_we & io_waddr == 14'h20)
        coreid <= {23'h0, coreid[8:0]};
      if (io_we & io_waddr == 14'h30)
        data0 <= io_wdata;
      if (io_we & io_waddr == 14'h31)
        data1 <= io_wdata;
      if (io_we & io_waddr == 14'h32)
        data2 <= io_wdata;
      if (io_we & io_waddr == 14'h33)
        data3 <= io_wdata;
      if (io_we & io_waddr == 14'h19)
        pgdl <= {io_wdata[31:12], 12'h0};
      if (io_we & io_waddr == 14'h1A)
        pgdh <= {io_wdata[31:12], 12'h0};
      if (io_we & io_waddr == 14'h1B)
        pgd <= {io_wdata[31:12], 12'h0};
      if (io_we & io_waddr == 14'h180)
        dmw0 <=
          {io_wdata[31:29],
           1'h0,
           io_wdata[27:25],
           19'h0,
           io_wdata[5:3],
           2'h0,
           io_wdata[0]};
      if (io_we & io_waddr == 14'h181)
        dmw1 <=
          {io_wdata[31:29],
           1'h0,
           io_wdata[27:25],
           19'h0,
           io_wdata[5:3],
           2'h0,
           io_wdata[0]};
      if (io_we & io_waddr == 14'h40)
        tid <= io_wdata;
      if (io_we & io_waddr == 14'h41) begin
        tcfg <= {2'h0, io_wdata[29:0]};
        tval <= {2'h0, io_wdata[29:2], 2'h1};
      end
      else if (tcfg[0]) begin
        if (_GEN_22)
          tval <= {2'h0, _tval_T_6};
        else
          tval <= 32'(tval - 32'h1);
      end
    end
    tval_edge <= tval;
  end // always @(posedge)
  assign io_rdata = io_raddr == 14'h0 ? crmd : _GEN_20;
  assign io_eentry_global = eentry;
  assign io_interrupt_vec =
    crmd[2] ? {estat[12:11] & ecfg[12:11], estat[9:0] & ecfg[9:0]} : 12'h0;
  assign io_crmd_trans = crmd[8:3];
  assign io_estat_13 = estat[12:0];
endmodule


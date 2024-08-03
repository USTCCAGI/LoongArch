// Generated by CIRCT firtool-1.62.0
module AXI_Arbiter(
  input         clock,
                reset,
  input  [31:0] io_i_araddr,
  input         io_i_rvalid,
  output        io_i_rready,
  output [31:0] io_i_rdata,
  output        io_i_rlast,
  input  [31:0] io_d_araddr,
  input         io_d_rvalid,
  output        io_d_rready,
  output [31:0] io_d_rdata,
  output        io_d_rlast,
  input  [31:0] io_d_awaddr,
  input         io_d_wvalid,
  output        io_d_wready,
  input  [31:0] io_d_wdata,
  input         io_d_wlast,
  output        io_d_bvalid,
  input         io_d_bready,
  output [31:0] io_araddr,
  output [7:0]  io_arlen,
  input         io_arready,
  output        io_arvalid,
  output [31:0] io_awaddr,
  input         io_awready,
  output        io_awvalid,
                io_bready,
  input         io_bvalid,
  input  [31:0] io_rdata,
  input         io_rlast,
  output        io_rready,
  input         io_rvalid,
  output [31:0] io_wdata,
  output        io_wlast,
  input         io_wready,
  output        io_wvalid
);

  reg  [2:0]      r_state;
  wire            _GEN = r_state == 3'h0;
  wire            _GEN_0 = r_state == 3'h1;
  wire            _GEN_1 = r_state == 3'h2;
  wire            _GEN_2 = _GEN | _GEN_0;
  wire            _GEN_3 = r_state == 3'h3;
  wire            _GEN_4 = _GEN | _GEN_0 | _GEN_1;
  wire            _GEN_5 = r_state == 3'h4;
  wire            _GEN_6 = _GEN | _GEN_0 | _GEN_1 | _GEN_3;
  wire            _GEN_7 = _GEN_1 ? io_i_rvalid : ~_GEN_3 & _GEN_5 & io_d_rvalid;
  reg  [1:0]      w_state;
  wire            _GEN_8 = w_state == 2'h0;
  wire            _GEN_9 = w_state == 2'h1;
  wire            _GEN_10 = w_state == 2'h2;
  wire            _GEN_11 = _GEN_8 | _GEN_9;
  wire            io_wvalid_0 = ~_GEN_11 & _GEN_10 & io_d_wvalid;
  wire            io_wlast_0 = ~_GEN_11 & _GEN_10 & io_d_wlast;
  wire            _GEN_12 = _GEN_8 | _GEN_9 | _GEN_10;
  wire            io_bready_0 = ~_GEN_12 & (&w_state) & io_d_bready;
  wire [2:0]      _r_state_T_5 = io_arready ? 3'h4 : 3'h3;
  wire [2:0]      _r_state_T_2 = io_arready ? 3'h2 : 3'h1;
  wire [2:0]      _r_state_T = io_d_rvalid ? 3'h3 : 3'h0;
  wire [2:0]      _r_state_T_1 = io_i_rvalid ? 3'h1 : _r_state_T;
  wire [7:0][2:0] _GEN_13 =
    {{r_state},
     {r_state},
     {r_state},
     {{~(io_d_rvalid & io_rlast), 2'h0}},
     {_r_state_T_5},
     {{1'h0, ~(io_i_rvalid & io_rlast), 1'h0}},
     {_r_state_T_2},
     {_r_state_T_1}};
  wire [1:0]      _w_state_T_6 = io_bready_0 & io_bvalid ? 2'h0 : 2'h3;
  wire [1:0]      _w_state_T_1 = io_awready ? 2'h2 : 2'h1;
  wire [3:0][1:0] _GEN_14 =
    {{_w_state_T_6},
     {{1'h1, io_wready & io_wlast_0 & io_wvalid_0}},
     {_w_state_T_1},
     {{1'h0, io_d_wvalid}}};
  always @(posedge clock) begin
    if (reset) begin
      r_state <= 3'h0;
      w_state <= 2'h0;
    end
    else begin
      r_state <= _GEN_13[r_state];
      w_state <= _GEN_14[w_state];
    end
  end // always @(posedge)
  assign io_i_rready = ~_GEN_2 & _GEN_1 & io_rvalid;
  assign io_i_rdata = io_rdata;
  assign io_i_rlast = ~_GEN_2 & _GEN_1 & io_rlast;
  assign io_d_rready = ~_GEN_6 & _GEN_5 & io_rvalid;
  assign io_d_rdata = io_rdata;
  assign io_d_rlast = ~_GEN_6 & _GEN_5 & io_rlast;
  assign io_d_wready = ~_GEN_11 & _GEN_10 & io_wready;
  assign io_d_bvalid = ~_GEN_12 & (&w_state) & io_bvalid;
  assign io_araddr = _GEN_4 | ~_GEN_3 ? io_i_araddr : io_d_araddr;
  assign io_arlen = _GEN_4 ? 8'hF : {4'h0, ~_GEN_3, 3'h7};
  assign io_arvalid = ~_GEN & (_GEN_0 | ~_GEN_1 & _GEN_3);
  assign io_awaddr = io_d_awaddr;
  assign io_awvalid = ~_GEN_8 & _GEN_9;
  assign io_bready = io_bready_0;
  assign io_rready = ~_GEN_2 & _GEN_7;
  assign io_wdata = io_d_wdata;
  assign io_wlast = io_wlast_0;
  assign io_wvalid = io_wvalid_0;
endmodule


// Generated by CIRCT firtool-1.62.0
module SB(
  input         clock,
                reset,
  output        io_full,
  input  [31:0] io_addr_ex,
                io_st_data_ex,
  input  [4:0]  io_mem_type_ex,
  input         io_uncache_ex,
  input  [1:0]  io_is_store_num_cmt,
  output        io_st_cmt_valid,
  input         io_dcache_miss,
  output [31:0] io_st_addr_cmt,
                io_st_data_cmt,
  output [1:0]  io_st_wlen_cmt,
  output        io_is_uncache_cmt,
  input         io_flush,
  output [31:0] io_ld_data_mem,
  output        io_ld_hit_0,
                io_ld_hit_1,
                io_ld_hit_2,
                io_ld_hit_3,
  input         io_em_stall
);

  reg  [31:0]      sb_0_addr;
  reg  [31:0]      sb_0_data;
  reg  [3:0]       sb_0_wstrb;
  reg              sb_0_uncache;
  reg  [31:0]      sb_1_addr;
  reg  [31:0]      sb_1_data;
  reg  [3:0]       sb_1_wstrb;
  reg              sb_1_uncache;
  reg  [31:0]      sb_2_addr;
  reg  [31:0]      sb_2_data;
  reg  [3:0]       sb_2_wstrb;
  reg              sb_2_uncache;
  reg  [31:0]      sb_3_addr;
  reg  [31:0]      sb_3_data;
  reg  [3:0]       sb_3_wstrb;
  reg              sb_3_uncache;
  reg  [1:0]       head;
  reg  [1:0]       rear;
  reg  [2:0]       size;
  reg              clear;
  wire             full = size == 3'h4 | clear;
  reg  [2:0]       waiting_cmt;
  wire [3:0][31:0] _GEN = {{sb_3_addr}, {sb_2_addr}, {sb_1_addr}, {sb_0_addr}};
  wire [3:0][31:0] _GEN_0 = {{sb_3_data}, {sb_2_data}, {sb_1_data}, {sb_0_data}};
  wire [3:0][3:0]  _GEN_1 = {{sb_3_wstrb}, {sb_2_wstrb}, {sb_1_wstrb}, {sb_0_wstrb}};
  wire [3:0]       _GEN_2 = _GEN_1[head];
  wire [3:0]       _GEN_3 =
    {{sb_3_uncache}, {sb_2_uncache}, {sb_1_uncache}, {sb_0_uncache}};
  wire [1:0]       _offset_T_5 = _GEN_2[1] ? 2'h1 : {1'h1, ~(_GEN_2[2])};
  wire [1:0]       offset = _GEN_2[0] ? 2'h0 : _offset_T_5;
  wire [2:0]       _io_st_wlen_cmt_T_9 =
    3'({1'h0, 2'({1'h0, _GEN_2[0]} + {1'h0, _GEN_2[1]})}
       + {1'h0, 2'({1'h0, _GEN_2[2]} + {1'h0, _GEN_2[3]})});
  reg  [31:0]      sb_order_reg_0_data;
  reg  [31:0]      sb_order_reg_1_data;
  reg  [31:0]      sb_order_reg_2_data;
  reg  [31:0]      sb_order_reg_3_data;
  reg  [3:0]       ld_hit_mask;
  reg  [3:0]       hit_index;
  wire [31:0]      _hit_byte_T_4 = hit_index[0] ? sb_order_reg_0_data : 32'h0;
  wire [31:0]      _hit_byte_T_5 = hit_index[1] ? sb_order_reg_1_data : 32'h0;
  wire [31:0]      _hit_byte_T_6 = hit_index[2] ? sb_order_reg_2_data : 32'h0;
  wire [31:0]      _hit_byte_T_7 = hit_index[3] ? sb_order_reg_3_data : 32'h0;
  reg  [1:0]       hit_byte_r;
  wire [31:0]      hit_byte =
    (_hit_byte_T_4 | _hit_byte_T_5 | _hit_byte_T_6 | _hit_byte_T_7)
    >> {27'h0, hit_byte_r, 3'h0};
  reg              ld_data_0_r;
  wire [7:0]       _ld_data_0_T_1 = ld_data_0_r ? hit_byte[7:0] : 8'h0;
  reg              io_ld_hit_0_r;
  reg  [3:0]       hit_index_1;
  wire [31:0]      _hit_byte_T_18 = hit_index_1[0] ? sb_order_reg_0_data : 32'h0;
  wire [31:0]      _hit_byte_T_19 = hit_index_1[1] ? sb_order_reg_1_data : 32'h0;
  wire [31:0]      _hit_byte_T_20 = hit_index_1[2] ? sb_order_reg_2_data : 32'h0;
  wire [31:0]      _hit_byte_T_21 = hit_index_1[3] ? sb_order_reg_3_data : 32'h0;
  reg  [1:0]       hit_byte_r_1;
  wire [31:0]      hit_byte_1 =
    (_hit_byte_T_18 | _hit_byte_T_19 | _hit_byte_T_20 | _hit_byte_T_21)
    >> {27'h0, hit_byte_r_1, 3'h0};
  reg              ld_data_1_r;
  wire [7:0]       _ld_data_1_T_1 = ld_data_1_r ? hit_byte_1[7:0] : 8'h0;
  reg              io_ld_hit_1_r;
  reg  [3:0]       hit_index_2;
  wire [31:0]      _hit_byte_T_32 = hit_index_2[0] ? sb_order_reg_0_data : 32'h0;
  wire [31:0]      _hit_byte_T_33 = hit_index_2[1] ? sb_order_reg_1_data : 32'h0;
  wire [31:0]      _hit_byte_T_34 = hit_index_2[2] ? sb_order_reg_2_data : 32'h0;
  wire [31:0]      _hit_byte_T_35 = hit_index_2[3] ? sb_order_reg_3_data : 32'h0;
  reg  [1:0]       hit_byte_r_2;
  wire [31:0]      hit_byte_2 =
    (_hit_byte_T_32 | _hit_byte_T_33 | _hit_byte_T_34 | _hit_byte_T_35)
    >> {27'h0, hit_byte_r_2, 3'h0};
  reg              ld_data_2_r;
  wire [7:0]       _ld_data_2_T_1 = ld_data_2_r ? hit_byte_2[7:0] : 8'h0;
  reg              io_ld_hit_2_r;
  reg  [3:0]       hit_index_3;
  wire [31:0]      _hit_byte_T_46 = hit_index_3[0] ? sb_order_reg_0_data : 32'h0;
  wire [31:0]      _hit_byte_T_47 = hit_index_3[1] ? sb_order_reg_1_data : 32'h0;
  wire [31:0]      _hit_byte_T_48 = hit_index_3[2] ? sb_order_reg_2_data : 32'h0;
  wire [31:0]      _hit_byte_T_49 = hit_index_3[3] ? sb_order_reg_3_data : 32'h0;
  reg  [1:0]       hit_byte_r_3;
  wire [31:0]      hit_byte_3 =
    (_hit_byte_T_46 | _hit_byte_T_47 | _hit_byte_T_48 | _hit_byte_T_49)
    >> {27'h0, hit_byte_r_3, 3'h0};
  reg              ld_data_3_r;
  wire [7:0]       _ld_data_3_T_1 = ld_data_3_r ? hit_byte_3[7:0] : 8'h0;
  reg              io_ld_hit_3_r;
  wire [1:0]       _start_T = 2'(head + waiting_cmt[1:0]);
  wire [2:0]       _clr_num_T = 3'(size - waiting_cmt);
  wire [1:0]       _GEN_4 = 2'(_start_T + 2'h1);
  wire             _GEN_5 =
    (|(_clr_num_T[2:1])) & _GEN_4 == 2'h0 | (|_clr_num_T) & _start_T == 2'h0;
  wire             _GEN_6 =
    (|(_clr_num_T[2:1])) & _GEN_4 == 2'h1 | (|_clr_num_T) & _start_T == 2'h1;
  wire             _GEN_7 =
    (|(_clr_num_T[2:1])) & _GEN_4 == 2'h2 | (|_clr_num_T) & _start_T == 2'h2;
  wire             _GEN_8 =
    (|(_clr_num_T[2:1])) & (&_GEN_4) | (|_clr_num_T) & (&_start_T);
  wire [1:0]       _GEN_9 = 2'(_start_T - 2'h2);
  wire             _GEN_10 = _clr_num_T > 3'h2;
  wire             _GEN_11 = _GEN_10 & _GEN_9 == 2'h0;
  wire             _GEN_12 = _GEN_10 & _GEN_9 == 2'h1;
  wire             _GEN_13 = _GEN_10 & _GEN_9 == 2'h2;
  wire             _GEN_14 = _GEN_10 & (&_GEN_9);
  wire [1:0]       _GEN_15 = 2'(_start_T - 2'h1);
  wire             _GEN_16 =
    _clr_num_T[2] ? _GEN_15 == 2'h0 | _GEN_11 | _GEN_5 : _GEN_11 | _GEN_5;
  wire             _GEN_17 =
    _clr_num_T[2] ? _GEN_15 == 2'h1 | _GEN_12 | _GEN_6 : _GEN_12 | _GEN_6;
  wire             _GEN_18 =
    _clr_num_T[2] ? _GEN_15 == 2'h2 | _GEN_13 | _GEN_7 : _GEN_13 | _GEN_7;
  wire             _GEN_19 =
    _clr_num_T[2] ? (&_GEN_15) | _GEN_14 | _GEN_8 : _GEN_14 | _GEN_8;
  wire [31:0]      _sb_addr_T_1 = {io_addr_ex[31:2], 2'h0};
  wire [62:0]      _sb_data_T_2 =
    {31'h0, io_st_data_ex} << {58'h0, io_addr_ex[1:0], 3'h0};
  wire [18:0]      _sb_wstrb_T_6 =
    {3'h0, 16'((16'h1 << (4'h1 << io_mem_type_ex[1:0])) - 16'h1)} << io_addr_ex[1:0];
  wire [3:0]       _GEN_20 = {2'h0, io_mem_type_ex[1:0]};
  wire [15:0]      _ld_mask_T_2 = 16'h1 << (4'h1 << _GEN_20);
  wire [3:0]       _ld_mask_T_3 = 4'(_ld_mask_T_2[3:0] - 4'h1);
  wire [1:0]       _sb_order_T_12 = 2'(rear - 2'h1);
  wire [1:0]       _sb_order_T_6 = 2'(rear - 2'h2);
  wire [1:0]       _sb_order_T_10 = 2'(rear + 2'h1);
  wire [3:0]       sb_order_3_wstrb = _GEN_1[rear];
  wire             ld_hit_temp_0 =
    (_GEN[_sb_order_T_12][31:2] ^ io_addr_ex[31:2]) == 30'h0;
  wire             ld_hit_temp_1 =
    (_GEN[_sb_order_T_6][31:2] ^ io_addr_ex[31:2]) == 30'h0;
  wire             ld_hit_temp_2 =
    (_GEN[_sb_order_T_10][31:2] ^ io_addr_ex[31:2]) == 30'h0;
  wire             ld_hit_temp_3 = (_GEN[rear][31:2] ^ io_addr_ex[31:2]) == 30'h0;
  wire [3:0]       _GEN_21 = {2'h0, io_addr_ex[1:0]};
  wire [3:0]       _hit_T_1 = _GEN_1[_sb_order_T_12] >> _GEN_21;
  wire             hit_0 = ld_hit_temp_0 & _hit_T_1[0];
  wire [3:0]       _hit_T_5 = _GEN_1[_sb_order_T_6] >> _GEN_21;
  wire             hit_1 = ld_hit_temp_1 & _hit_T_5[0];
  wire [3:0]       _hit_T_9 = _GEN_1[_sb_order_T_10] >> _GEN_21;
  wire             hit_2 = ld_hit_temp_2 & _hit_T_9[0];
  wire [3:0]       _hit_T_13 = sb_order_3_wstrb >> _GEN_21;
  wire             hit_3 = ld_hit_temp_3 & _hit_T_13[0];
  wire             bit_hit = (|{hit_3, hit_2, hit_1, hit_0}) & _ld_mask_T_3[0];
  wire [1:0]       _addr_T_7 = 2'(io_addr_ex[1:0] + 2'h1);
  wire [3:0]       _GEN_22 = {2'h0, _addr_T_7};
  wire [3:0]       _hit_T_17 = _GEN_1[_sb_order_T_12] >> _GEN_22;
  wire             hit_1_0 = ld_hit_temp_0 & _hit_T_17[0];
  wire [3:0]       _hit_T_21 = _GEN_1[_sb_order_T_6] >> _GEN_22;
  wire             hit_1_1 = ld_hit_temp_1 & _hit_T_21[0];
  wire [3:0]       _hit_T_25 = _GEN_1[_sb_order_T_10] >> _GEN_22;
  wire             hit_1_2 = ld_hit_temp_2 & _hit_T_25[0];
  wire [3:0]       _hit_T_29 = sb_order_3_wstrb >> _GEN_22;
  wire             hit_1_3 = ld_hit_temp_3 & _hit_T_29[0];
  wire             bit_hit_1 = (|{hit_1_3, hit_1_2, hit_1_1, hit_1_0}) & _ld_mask_T_3[1];
  wire [1:0]       _addr_T_12 = 2'(io_addr_ex[1:0] - 2'h2);
  wire [3:0]       _GEN_23 = {2'h0, _addr_T_12};
  wire [3:0]       _hit_T_33 = _GEN_1[_sb_order_T_12] >> _GEN_23;
  wire             hit_2_0 = ld_hit_temp_0 & _hit_T_33[0];
  wire [3:0]       _hit_T_37 = _GEN_1[_sb_order_T_6] >> _GEN_23;
  wire             hit_2_1 = ld_hit_temp_1 & _hit_T_37[0];
  wire [3:0]       _hit_T_41 = _GEN_1[_sb_order_T_10] >> _GEN_23;
  wire             hit_2_2 = ld_hit_temp_2 & _hit_T_41[0];
  wire [3:0]       _hit_T_45 = sb_order_3_wstrb >> _GEN_23;
  wire             hit_2_3 = ld_hit_temp_3 & _hit_T_45[0];
  wire             bit_hit_2 = (|{hit_2_3, hit_2_2, hit_2_1, hit_2_0}) & _ld_mask_T_3[2];
  wire [1:0]       _addr_T_17 = 2'(io_addr_ex[1:0] - 2'h1);
  wire [3:0]       _GEN_24 = {2'h0, _addr_T_17};
  wire [3:0]       _hit_T_49 = _GEN_1[_sb_order_T_12] >> _GEN_24;
  wire             hit_3_0 = ld_hit_temp_0 & _hit_T_49[0];
  wire [3:0]       _hit_T_53 = _GEN_1[_sb_order_T_6] >> _GEN_24;
  wire             hit_3_1 = ld_hit_temp_1 & _hit_T_53[0];
  wire [3:0]       _hit_T_57 = _GEN_1[_sb_order_T_10] >> _GEN_24;
  wire             hit_3_2 = ld_hit_temp_2 & _hit_T_57[0];
  wire [3:0]       _hit_T_61 = sb_order_3_wstrb >> _GEN_24;
  wire             hit_3_3 = ld_hit_temp_3 & _hit_T_61[0];
  wire             bit_hit_3 = (|{hit_3_3, hit_3_2, hit_3_1, hit_3_0}) & _ld_mask_T_3[3];
  wire [18:0]      _ld_hit_mask_T_3 = 19'hF << (4'h1 << _GEN_20);
  wire [3:0]       _hit_index_T_6 = hit_2 ? 4'h4 : {hit_3, 3'h0};
  wire [3:0]       _hit_index_T_7 = hit_1 ? 4'h2 : _hit_index_T_6;
  wire [3:0]       _hit_index_T_16 = hit_1_2 ? 4'h4 : {hit_1_3, 3'h0};
  wire [3:0]       _hit_index_T_17 = hit_1_1 ? 4'h2 : _hit_index_T_16;
  wire [3:0]       _hit_index_T_26 = hit_2_2 ? 4'h4 : {hit_2_3, 3'h0};
  wire [3:0]       _hit_index_T_27 = hit_2_1 ? 4'h2 : _hit_index_T_26;
  wire [3:0]       _hit_index_T_36 = hit_3_2 ? 4'h4 : {hit_3_3, 3'h0};
  wire [3:0]       _hit_index_T_37 = hit_3_1 ? 4'h2 : _hit_index_T_36;
  wire             _GEN_25 = ~clear & io_mem_type_ex[4];
  wire             _GEN_26 = _GEN_25 & rear == 2'h0;
  wire             _GEN_27 = clear | ~_GEN_26;
  wire             _GEN_28 = _GEN_25 & rear == 2'h1;
  wire             _GEN_29 = _GEN_25 & rear == 2'h2;
  wire             _GEN_30 = _GEN_25 & (&rear);
  always @(posedge clock) begin
    if (reset) begin
      sb_0_addr <= 32'h0;
      sb_0_data <= 32'h0;
      sb_0_wstrb <= 4'h0;
      sb_0_uncache <= 1'h0;
      sb_1_addr <= 32'h0;
      sb_1_data <= 32'h0;
      sb_1_wstrb <= 4'h0;
      sb_1_uncache <= 1'h0;
      sb_2_addr <= 32'h0;
      sb_2_data <= 32'h0;
      sb_2_wstrb <= 4'h0;
      sb_2_uncache <= 1'h0;
      sb_3_addr <= 32'h0;
      sb_3_data <= 32'h0;
      sb_3_wstrb <= 4'h0;
      sb_3_uncache <= 1'h0;
      head <= 2'h0;
      rear <= 2'h0;
      size <= 3'h0;
      clear <= 1'h0;
      waiting_cmt <= 3'h0;
    end
    else begin
      if (_GEN_27) begin
      end
      else begin
        sb_0_addr <= _sb_addr_T_1;
        sb_0_data <= _sb_data_T_2[31:0];
      end
      if (clear) begin
        if (_GEN_16)
          sb_0_wstrb <= 4'h0;
        if (_GEN_17)
          sb_1_wstrb <= 4'h0;
        if (_GEN_18)
          sb_2_wstrb <= 4'h0;
        if (_GEN_19)
          sb_3_wstrb <= 4'h0;
      end
      else begin
        if (_GEN_26)
          sb_0_wstrb <= _sb_wstrb_T_6[3:0];
        if (_GEN_28)
          sb_1_wstrb <= _sb_wstrb_T_6[3:0];
        if (_GEN_29)
          sb_2_wstrb <= _sb_wstrb_T_6[3:0];
        if (_GEN_30)
          sb_3_wstrb <= _sb_wstrb_T_6[3:0];
      end
      if (_GEN_27) begin
      end
      else
        sb_0_uncache <= io_uncache_ex;
      if (clear | ~_GEN_28) begin
      end
      else begin
        sb_1_addr <= _sb_addr_T_1;
        sb_1_data <= _sb_data_T_2[31:0];
        sb_1_uncache <= io_uncache_ex;
      end
      if (clear | ~_GEN_29) begin
      end
      else begin
        sb_2_addr <= _sb_addr_T_1;
        sb_2_data <= _sb_data_T_2[31:0];
        sb_2_uncache <= io_uncache_ex;
      end
      if (clear | ~_GEN_30) begin
      end
      else begin
        sb_3_addr <= _sb_addr_T_1;
        sb_3_data <= _sb_data_T_2[31:0];
        sb_3_uncache <= io_uncache_ex;
      end
      head <= 2'(head + {1'h0, (|waiting_cmt) & ~io_dcache_miss});
      if (clear & ~(|waiting_cmt))
        rear <= 2'(head + {1'h0, (|waiting_cmt) & ~io_dcache_miss});
      else
        rear <= 2'(rear + {1'h0, io_mem_type_ex[4]});
      if (clear & ~(|waiting_cmt))
        size <= 3'h0;
      else
        size <=
          3'(3'(size + {2'h0, io_mem_type_ex[4] & ~full})
             - {2'h0, (|waiting_cmt) & ~io_dcache_miss});
      clear <= io_flush | (|waiting_cmt) & clear;
      waiting_cmt <=
        3'(3'(waiting_cmt + {1'h0, io_is_store_num_cmt})
           - {2'h0, (|waiting_cmt) & ~io_dcache_miss});
    end
    if (io_em_stall) begin
    end
    else begin
      sb_order_reg_0_data <= _GEN_0[_sb_order_T_12];
      sb_order_reg_1_data <= _GEN_0[_sb_order_T_6];
      sb_order_reg_2_data <= _GEN_0[_sb_order_T_10];
      sb_order_reg_3_data <= _GEN_0[rear];
      ld_hit_mask <= io_mem_type_ex[4] ? 4'hF : _ld_hit_mask_T_3[3:0];
      hit_index <= hit_0 ? 4'h1 : _hit_index_T_7;
      hit_byte_r <= io_addr_ex[1:0];
      ld_data_0_r <= bit_hit;
      io_ld_hit_0_r <= bit_hit;
      hit_index_1 <= hit_1_0 ? 4'h1 : _hit_index_T_17;
      hit_byte_r_1 <= _addr_T_7;
      ld_data_1_r <= bit_hit_1;
      io_ld_hit_1_r <= bit_hit_1;
      hit_index_2 <= hit_2_0 ? 4'h1 : _hit_index_T_27;
      hit_byte_r_2 <= _addr_T_12;
      ld_data_2_r <= bit_hit_2;
      io_ld_hit_2_r <= bit_hit_2;
      hit_index_3 <= hit_3_0 ? 4'h1 : _hit_index_T_37;
      hit_byte_r_3 <= _addr_T_17;
      ld_data_3_r <= bit_hit_3;
      io_ld_hit_3_r <= bit_hit_3;
    end
  end // always @(posedge)
  assign io_full = full;
  assign io_st_cmt_valid = |waiting_cmt;
  assign io_st_addr_cmt = 32'(_GEN[head] + {30'h0, offset});
  assign io_st_data_cmt = _GEN_0[head] >> {27'h0, offset, 3'h0};
  assign io_st_wlen_cmt = _io_st_wlen_cmt_T_9[2:1];
  assign io_is_uncache_cmt = _GEN_3[head];
  assign io_ld_data_mem =
    {_ld_data_3_T_1, _ld_data_2_T_1, _ld_data_1_T_1, _ld_data_0_T_1};
  assign io_ld_hit_0 = ld_hit_mask[0] | io_ld_hit_0_r;
  assign io_ld_hit_1 = ld_hit_mask[1] | io_ld_hit_1_r;
  assign io_ld_hit_2 = ld_hit_mask[2] | io_ld_hit_2_r;
  assign io_ld_hit_3 = ld_hit_mask[3] | io_ld_hit_3_r;
endmodule


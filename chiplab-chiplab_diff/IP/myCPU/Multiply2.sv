// Generated by CIRCT firtool-1.62.0
module Multiply2(
  input         clock,
  input  [31:0] io_num1,
                io_num2,
  input  [4:0]  io_op,
  output [31:0] io_mul_out,
  input         io_busy_6,
                io_busy_7,
                io_busy_8,
                io_busy_9,
                io_busy_10
);

  reg  [65:0]      Num_reg1_0;
  reg  [65:0]      Num_reg1_1;
  reg  [65:0]      Num_reg1_2;
  reg  [65:0]      Num_reg1_3;
  reg  [65:0]      Num_reg1_4;
  reg  [65:0]      Num_reg1_5;
  reg  [65:0]      Num_reg1_6;
  reg  [65:0]      Num_reg1_7;
  reg  [65:0]      Num_reg1_8;
  reg  [65:0]      Num_reg1_9;
  reg  [65:0]      Num_reg1_10;
  reg  [65:0]      Num_reg1_11;
  reg  [65:0]      Num_reg1_12;
  reg  [65:0]      Num_reg1_13;
  reg  [65:0]      Num_reg1_14;
  reg  [65:0]      Num_reg1_15;
  reg  [65:0]      Num_reg1_16;
  reg  [65:0]      Num_reg1_17;
  reg  [65:0]      Num_reg1_18;
  reg  [65:0]      Num_reg1_19;
  reg  [65:0]      Num_reg1_20;
  reg  [65:0]      Num_reg1_21;
  reg  [65:0]      Num_reg1_22;
  reg  [65:0]      Num_reg1_23;
  reg  [65:0]      Num_reg1_24;
  reg  [65:0]      Num_reg1_25;
  reg  [65:0]      Num_reg1_26;
  reg  [65:0]      Num_reg1_27;
  reg  [65:0]      Num_reg1_28;
  reg  [65:0]      Num_reg1_29;
  reg  [65:0]      Num_reg1_30;
  reg  [65:0]      Num_reg1_31;
  reg  [65:0]      Num_reg1_32;
  reg  [63:0]      mul_out1;
  reg  [63:0]      mul_out2;
  reg  [4:0]       op_reg1;
  reg  [4:0]       op_reg2;
  wire [63:0]      _mul_out_T = 64'(mul_out1 + mul_out2);
  wire             _GEN = io_op == 5'h2;
  wire [33:0]      _GEN_0 = _GEN ? 34'h0 : {34{io_num1[31]}};
  wire [65:0]      num1 = {_GEN_0, io_num1};
  wire [33:0]      _GEN_1 = _GEN ? 34'h0 : {34{io_num2[31]}};
  wire [2:0]       booth_16 = {_GEN_1[1:0], io_num2[31]};
  wire [3:0][65:0] _GEN_2 =
    {{66'(~num1 + 66'h1)},
     {66'({~(_GEN_0[32:0]), ~io_num1, 1'h1} + 66'h1)},
     {num1},
     {66'h0}};
  wire [65:0]      _GEN_3 =
    io_num2[3:1] == 3'h5 | io_num2[3:1] == 3'h6
      ? 66'({~(_GEN_0[31:0]), ~io_num1, 2'h3} + 66'h1)
      : 66'h0;
  wire [65:0]      _GEN_4 =
    io_num2[3:1] == 3'h4 ? 66'({~(_GEN_0[30:0]), ~io_num1, 3'h7} + 66'h1) : _GEN_3;
  wire [65:0]      _GEN_5 = io_num2[3:1] == 3'h3 ? {_GEN_0[30:0], io_num1, 3'h0} : _GEN_4;
  wire [65:0]      _GEN_6 =
    io_num2[5:3] == 3'h5 | io_num2[5:3] == 3'h6
      ? 66'({~(_GEN_0[29:0]), ~io_num1, 4'hF} + 66'h1)
      : 66'h0;
  wire [65:0]      _GEN_7 =
    io_num2[5:3] == 3'h4 ? 66'({~(_GEN_0[28:0]), ~io_num1, 5'h1F} + 66'h1) : _GEN_6;
  wire [65:0]      _GEN_8 = io_num2[5:3] == 3'h3 ? {_GEN_0[28:0], io_num1, 5'h0} : _GEN_7;
  wire [65:0]      _GEN_9 =
    io_num2[7:5] == 3'h5 | io_num2[7:5] == 3'h6
      ? 66'({~(_GEN_0[27:0]), ~io_num1, 6'h3F} + 66'h1)
      : 66'h0;
  wire [65:0]      _GEN_10 =
    io_num2[7:5] == 3'h4 ? 66'({~(_GEN_0[26:0]), ~io_num1, 7'h7F} + 66'h1) : _GEN_9;
  wire [65:0]      _GEN_11 =
    io_num2[7:5] == 3'h3 ? {_GEN_0[26:0], io_num1, 7'h0} : _GEN_10;
  wire [65:0]      _GEN_12 =
    io_num2[9:7] == 3'h5 | io_num2[9:7] == 3'h6
      ? 66'({~(_GEN_0[25:0]), ~io_num1, 8'hFF} + 66'h1)
      : 66'h0;
  wire [65:0]      _GEN_13 =
    io_num2[9:7] == 3'h4 ? 66'({~(_GEN_0[24:0]), ~io_num1, 9'h1FF} + 66'h1) : _GEN_12;
  wire [65:0]      _GEN_14 =
    io_num2[9:7] == 3'h3 ? {_GEN_0[24:0], io_num1, 9'h0} : _GEN_13;
  wire [65:0]      _GEN_15 =
    io_num2[11:9] == 3'h5 | io_num2[11:9] == 3'h6
      ? 66'({~(_GEN_0[23:0]), ~io_num1, 10'h3FF} + 66'h1)
      : 66'h0;
  wire [65:0]      _GEN_16 =
    io_num2[11:9] == 3'h4 ? 66'({~(_GEN_0[22:0]), ~io_num1, 11'h7FF} + 66'h1) : _GEN_15;
  wire [65:0]      _GEN_17 =
    io_num2[11:9] == 3'h3 ? {_GEN_0[22:0], io_num1, 11'h0} : _GEN_16;
  wire [65:0]      _GEN_18 =
    io_num2[13:11] == 3'h5 | io_num2[13:11] == 3'h6
      ? 66'({~(_GEN_0[21:0]), ~io_num1, 12'hFFF} + 66'h1)
      : 66'h0;
  wire [65:0]      _GEN_19 =
    io_num2[13:11] == 3'h4 ? 66'({~(_GEN_0[20:0]), ~io_num1, 13'h1FFF} + 66'h1) : _GEN_18;
  wire [65:0]      _GEN_20 =
    io_num2[13:11] == 3'h3 ? {_GEN_0[20:0], io_num1, 13'h0} : _GEN_19;
  wire [65:0]      _GEN_21 =
    io_num2[15:13] == 3'h5 | io_num2[15:13] == 3'h6
      ? 66'({~(_GEN_0[19:0]), ~io_num1, 14'h3FFF} + 66'h1)
      : 66'h0;
  wire [65:0]      _GEN_22 =
    io_num2[15:13] == 3'h4 ? 66'({~(_GEN_0[18:0]), ~io_num1, 15'h7FFF} + 66'h1) : _GEN_21;
  wire [65:0]      _GEN_23 =
    io_num2[15:13] == 3'h3 ? {_GEN_0[18:0], io_num1, 15'h0} : _GEN_22;
  wire [65:0]      _GEN_24 =
    io_num2[17:15] == 3'h5 | io_num2[17:15] == 3'h6
      ? 66'({~(_GEN_0[17:0]), ~io_num1, 16'hFFFF} + 66'h1)
      : 66'h0;
  wire [65:0]      _GEN_25 =
    io_num2[17:15] == 3'h4
      ? 66'({~(_GEN_0[16:0]), ~io_num1, 17'h1FFFF} + 66'h1)
      : _GEN_24;
  wire [65:0]      _GEN_26 =
    io_num2[17:15] == 3'h3 ? {_GEN_0[16:0], io_num1, 17'h0} : _GEN_25;
  wire [65:0]      _GEN_27 =
    io_num2[19:17] == 3'h5 | io_num2[19:17] == 3'h6
      ? 66'({~(_GEN_0[15:0]), ~io_num1, 18'h3FFFF} + 66'h1)
      : 66'h0;
  wire [65:0]      _GEN_28 =
    io_num2[19:17] == 3'h4
      ? 66'({~(_GEN_0[14:0]), ~io_num1, 19'h7FFFF} + 66'h1)
      : _GEN_27;
  wire [65:0]      _GEN_29 =
    io_num2[19:17] == 3'h3 ? {_GEN_0[14:0], io_num1, 19'h0} : _GEN_28;
  wire [65:0]      _GEN_30 =
    io_num2[21:19] == 3'h5 | io_num2[21:19] == 3'h6
      ? 66'({~(_GEN_0[13:0]), ~io_num1, 20'hFFFFF} + 66'h1)
      : 66'h0;
  wire [65:0]      _GEN_31 =
    io_num2[21:19] == 3'h4
      ? 66'({~(_GEN_0[12:0]), ~io_num1, 21'h1FFFFF} + 66'h1)
      : _GEN_30;
  wire [65:0]      _GEN_32 =
    io_num2[21:19] == 3'h3 ? {_GEN_0[12:0], io_num1, 21'h0} : _GEN_31;
  wire [65:0]      _GEN_33 =
    io_num2[23:21] == 3'h5 | io_num2[23:21] == 3'h6
      ? 66'({~(_GEN_0[11:0]), ~io_num1, 22'h3FFFFF} + 66'h1)
      : 66'h0;
  wire [65:0]      _GEN_34 =
    io_num2[23:21] == 3'h4
      ? 66'({~(_GEN_0[10:0]), ~io_num1, 23'h7FFFFF} + 66'h1)
      : _GEN_33;
  wire [65:0]      _GEN_35 =
    io_num2[23:21] == 3'h3 ? {_GEN_0[10:0], io_num1, 23'h0} : _GEN_34;
  wire [65:0]      _GEN_36 =
    io_num2[25:23] == 3'h5 | io_num2[25:23] == 3'h6
      ? 66'({~(_GEN_0[9:0]), ~io_num1, 24'hFFFFFF} + 66'h1)
      : 66'h0;
  wire [65:0]      _GEN_37 =
    io_num2[25:23] == 3'h4
      ? 66'({~(_GEN_0[8:0]), ~io_num1, 25'h1FFFFFF} + 66'h1)
      : _GEN_36;
  wire [65:0]      _GEN_38 =
    io_num2[25:23] == 3'h3 ? {_GEN_0[8:0], io_num1, 25'h0} : _GEN_37;
  wire [65:0]      _GEN_39 =
    io_num2[27:25] == 3'h5 | io_num2[27:25] == 3'h6
      ? 66'({~(_GEN_0[7:0]), ~io_num1, 26'h3FFFFFF} + 66'h1)
      : 66'h0;
  wire [65:0]      _GEN_40 =
    io_num2[27:25] == 3'h4
      ? 66'({~(_GEN_0[6:0]), ~io_num1, 27'h7FFFFFF} + 66'h1)
      : _GEN_39;
  wire [65:0]      _GEN_41 =
    io_num2[27:25] == 3'h3 ? {_GEN_0[6:0], io_num1, 27'h0} : _GEN_40;
  wire [65:0]      _GEN_42 =
    io_num2[29:27] == 3'h5 | io_num2[29:27] == 3'h6
      ? 66'({~(_GEN_0[5:0]), ~io_num1, 28'hFFFFFFF} + 66'h1)
      : 66'h0;
  wire [65:0]      _GEN_43 =
    io_num2[29:27] == 3'h4
      ? 66'({~(_GEN_0[4:0]), ~io_num1, 29'h1FFFFFFF} + 66'h1)
      : _GEN_42;
  wire [65:0]      _GEN_44 =
    io_num2[29:27] == 3'h3 ? {_GEN_0[4:0], io_num1, 29'h0} : _GEN_43;
  wire [65:0]      _GEN_45 =
    io_num2[31:29] == 3'h5 | io_num2[31:29] == 3'h6
      ? 66'({~(_GEN_0[3:0]), ~io_num1, 30'h3FFFFFFF} + 66'h1)
      : 66'h0;
  wire [65:0]      _GEN_46 =
    io_num2[31:29] == 3'h4
      ? 66'({~(_GEN_0[2:0]), ~io_num1, 31'h7FFFFFFF} + 66'h1)
      : _GEN_45;
  wire [65:0]      _GEN_47 =
    io_num2[31:29] == 3'h3 ? {_GEN_0[2:0], io_num1, 31'h0} : _GEN_46;
  wire [65:0]      _GEN_48 =
    booth_16 == 3'h5 | booth_16 == 3'h6
      ? 66'({~(_GEN_0[1:0]), ~io_num1, 32'hFFFFFFFF} + 66'h1)
      : 66'h0;
  wire [65:0]      _GEN_49 =
    booth_16 == 3'h4 ? 66'({~(_GEN_0[0]), ~io_num1, 33'h1FFFFFFFF} + 66'h1) : _GEN_48;
  wire [65:0]      _GEN_50 = booth_16 == 3'h3 ? {_GEN_0[0], io_num1, 33'h0} : _GEN_49;
  wire [65:0]      _GEN_51 =
    _GEN_1[3:1] == 3'h5 | _GEN_1[3:1] == 3'h6
      ? 66'({~io_num1, 34'h3FFFFFFFF} + 66'h1)
      : 66'h0;
  wire [65:0]      _GEN_52 =
    _GEN_1[3:1] == 3'h4 ? 66'({~(io_num1[30:0]), 35'h7FFFFFFFF} + 66'h1) : _GEN_51;
  wire [65:0]      _GEN_53 = _GEN_1[3:1] == 3'h3 ? {io_num1[30:0], 35'h0} : _GEN_52;
  wire [65:0]      _GEN_54 =
    _GEN_1[5:3] == 3'h5 | _GEN_1[5:3] == 3'h6
      ? 66'({~(io_num1[29:0]), 36'hFFFFFFFFF} + 66'h1)
      : 66'h0;
  wire [65:0]      _GEN_55 =
    _GEN_1[5:3] == 3'h4 ? 66'({~(io_num1[28:0]), 37'h1FFFFFFFFF} + 66'h1) : _GEN_54;
  wire [65:0]      _GEN_56 = _GEN_1[5:3] == 3'h3 ? {io_num1[28:0], 37'h0} : _GEN_55;
  wire [65:0]      _GEN_57 =
    _GEN_1[7:5] == 3'h5 | _GEN_1[7:5] == 3'h6
      ? 66'({~(io_num1[27:0]), 38'h3FFFFFFFFF} + 66'h1)
      : 66'h0;
  wire [65:0]      _GEN_58 =
    _GEN_1[7:5] == 3'h4 ? 66'({~(io_num1[26:0]), 39'h7FFFFFFFFF} + 66'h1) : _GEN_57;
  wire [65:0]      _GEN_59 = _GEN_1[7:5] == 3'h3 ? {io_num1[26:0], 39'h0} : _GEN_58;
  wire [65:0]      _GEN_60 =
    _GEN_1[9:7] == 3'h5 | _GEN_1[9:7] == 3'h6
      ? 66'({~(io_num1[25:0]), 40'hFFFFFFFFFF} + 66'h1)
      : 66'h0;
  wire [65:0]      _GEN_61 =
    _GEN_1[9:7] == 3'h4 ? 66'({~(io_num1[24:0]), 41'h1FFFFFFFFFF} + 66'h1) : _GEN_60;
  wire [65:0]      _GEN_62 = _GEN_1[9:7] == 3'h3 ? {io_num1[24:0], 41'h0} : _GEN_61;
  wire [65:0]      _GEN_63 =
    _GEN_1[11:9] == 3'h5 | _GEN_1[11:9] == 3'h6
      ? 66'({~(io_num1[23:0]), 42'h3FFFFFFFFFF} + 66'h1)
      : 66'h0;
  wire [65:0]      _GEN_64 =
    _GEN_1[11:9] == 3'h4 ? 66'({~(io_num1[22:0]), 43'h7FFFFFFFFFF} + 66'h1) : _GEN_63;
  wire [65:0]      _GEN_65 = _GEN_1[11:9] == 3'h3 ? {io_num1[22:0], 43'h0} : _GEN_64;
  wire [65:0]      _GEN_66 =
    _GEN_1[13:11] == 3'h5 | _GEN_1[13:11] == 3'h6
      ? 66'({~(io_num1[21:0]), 44'hFFFFFFFFFFF} + 66'h1)
      : 66'h0;
  wire [65:0]      _GEN_67 =
    _GEN_1[13:11] == 3'h4 ? 66'({~(io_num1[20:0]), 45'h1FFFFFFFFFFF} + 66'h1) : _GEN_66;
  wire [65:0]      _GEN_68 = _GEN_1[13:11] == 3'h3 ? {io_num1[20:0], 45'h0} : _GEN_67;
  wire [65:0]      _GEN_69 =
    _GEN_1[15:13] == 3'h5 | _GEN_1[15:13] == 3'h6
      ? 66'({~(io_num1[19:0]), 46'h3FFFFFFFFFFF} + 66'h1)
      : 66'h0;
  wire [65:0]      _GEN_70 =
    _GEN_1[15:13] == 3'h4 ? 66'({~(io_num1[18:0]), 47'h7FFFFFFFFFFF} + 66'h1) : _GEN_69;
  wire [65:0]      _GEN_71 = _GEN_1[15:13] == 3'h3 ? {io_num1[18:0], 47'h0} : _GEN_70;
  wire [65:0]      _GEN_72 =
    _GEN_1[17:15] == 3'h5 | _GEN_1[17:15] == 3'h6
      ? 66'({~(io_num1[17:0]), 48'hFFFFFFFFFFFF} + 66'h1)
      : 66'h0;
  wire [65:0]      _GEN_73 =
    _GEN_1[17:15] == 3'h4 ? 66'({~(io_num1[16:0]), 49'h1FFFFFFFFFFFF} + 66'h1) : _GEN_72;
  wire [65:0]      _GEN_74 = _GEN_1[17:15] == 3'h3 ? {io_num1[16:0], 49'h0} : _GEN_73;
  wire [65:0]      _GEN_75 =
    _GEN_1[19:17] == 3'h5 | _GEN_1[19:17] == 3'h6
      ? 66'({~(io_num1[15:0]), 50'h3FFFFFFFFFFFF} + 66'h1)
      : 66'h0;
  wire [65:0]      _GEN_76 =
    _GEN_1[19:17] == 3'h4 ? 66'({~(io_num1[14:0]), 51'h7FFFFFFFFFFFF} + 66'h1) : _GEN_75;
  wire [65:0]      _GEN_77 = _GEN_1[19:17] == 3'h3 ? {io_num1[14:0], 51'h0} : _GEN_76;
  wire [65:0]      _GEN_78 =
    _GEN_1[21:19] == 3'h5 | _GEN_1[21:19] == 3'h6
      ? 66'({~(io_num1[13:0]), 52'hFFFFFFFFFFFFF} + 66'h1)
      : 66'h0;
  wire [65:0]      _GEN_79 =
    _GEN_1[21:19] == 3'h4 ? 66'({~(io_num1[12:0]), 53'h1FFFFFFFFFFFFF} + 66'h1) : _GEN_78;
  wire [65:0]      _GEN_80 = _GEN_1[21:19] == 3'h3 ? {io_num1[12:0], 53'h0} : _GEN_79;
  wire [65:0]      _GEN_81 =
    _GEN_1[23:21] == 3'h5 | _GEN_1[23:21] == 3'h6
      ? 66'({~(io_num1[11:0]), 54'h3FFFFFFFFFFFFF} + 66'h1)
      : 66'h0;
  wire [65:0]      _GEN_82 =
    _GEN_1[23:21] == 3'h4 ? 66'({~(io_num1[10:0]), 55'h7FFFFFFFFFFFFF} + 66'h1) : _GEN_81;
  wire [65:0]      _GEN_83 = _GEN_1[23:21] == 3'h3 ? {io_num1[10:0], 55'h0} : _GEN_82;
  wire [65:0]      _GEN_84 =
    _GEN_1[25:23] == 3'h5 | _GEN_1[25:23] == 3'h6
      ? 66'({~(io_num1[9:0]), 56'hFFFFFFFFFFFFFF} + 66'h1)
      : 66'h0;
  wire [65:0]      _GEN_85 =
    _GEN_1[25:23] == 3'h4 ? 66'({~(io_num1[8:0]), 57'h1FFFFFFFFFFFFFF} + 66'h1) : _GEN_84;
  wire [65:0]      _GEN_86 = _GEN_1[25:23] == 3'h3 ? {io_num1[8:0], 57'h0} : _GEN_85;
  wire [65:0]      _GEN_87 =
    _GEN_1[27:25] == 3'h5 | _GEN_1[27:25] == 3'h6
      ? 66'({~(io_num1[7:0]), 58'h3FFFFFFFFFFFFFF} + 66'h1)
      : 66'h0;
  wire [65:0]      _GEN_88 =
    _GEN_1[27:25] == 3'h4 ? 66'({~(io_num1[6:0]), 59'h7FFFFFFFFFFFFFF} + 66'h1) : _GEN_87;
  wire [65:0]      _GEN_89 = _GEN_1[27:25] == 3'h3 ? {io_num1[6:0], 59'h0} : _GEN_88;
  wire [65:0]      _GEN_90 =
    _GEN_1[29:27] == 3'h5 | _GEN_1[29:27] == 3'h6
      ? 66'({~(io_num1[5:0]), 60'hFFFFFFFFFFFFFFF} + 66'h1)
      : 66'h0;
  wire [65:0]      _GEN_91 =
    _GEN_1[29:27] == 3'h4
      ? 66'({~(io_num1[4:0]), 61'h1FFFFFFFFFFFFFFF} + 66'h1)
      : _GEN_90;
  wire [65:0]      _GEN_92 = _GEN_1[29:27] == 3'h3 ? {io_num1[4:0], 61'h0} : _GEN_91;
  wire [65:0]      _GEN_93 =
    _GEN_1[31:29] == 3'h5 | _GEN_1[31:29] == 3'h6
      ? 66'({~(io_num1[3:0]), 62'h3FFFFFFFFFFFFFFF} + 66'h1)
      : 66'h0;
  wire [65:0]      _GEN_94 =
    _GEN_1[31:29] == 3'h4
      ? 66'({~(io_num1[2:0]), 63'h7FFFFFFFFFFFFFFF} + 66'h1)
      : _GEN_93;
  wire [65:0]      _GEN_95 = _GEN_1[31:29] == 3'h3 ? {io_num1[2:0], 63'h0} : _GEN_94;
  wire [65:0]      _GEN_96 =
    _GEN_1[33:31] == 3'h5 | _GEN_1[33:31] == 3'h6
      ? 66'({~(io_num1[1:0]), 64'hFFFFFFFFFFFFFFFF} + 66'h1)
      : 66'h0;
  wire [65:0]      _GEN_97 =
    _GEN_1[33:31] == 3'h4 ? 66'({~(io_num1[0]), 65'h1FFFFFFFFFFFFFFFF} + 66'h1) : _GEN_96;
  wire [65:0]      _GEN_98 = _GEN_1[33:31] == 3'h3 ? {io_num1[0], 65'h0} : _GEN_97;
  wire [63:0]      _input_1_0_mul_out2_T_1 = Num_reg1_0[63:0] ^ Num_reg1_1[63:0];
  wire [63:0]      input_1_0_mul_out1 = _input_1_0_mul_out2_T_1 ^ Num_reg1_2[63:0];
  wire [62:0]      _input_1_0_mul_out2_T_3 =
    Num_reg1_0[62:0] & Num_reg1_1[62:0] | Num_reg1_2[62:0]
    & _input_1_0_mul_out2_T_1[62:0];
  wire [63:0]      _input_1_1_mul_out2_T_1 = Num_reg1_3[63:0] ^ Num_reg1_4[63:0];
  wire [63:0]      input_1_1_mul_out1 = _input_1_1_mul_out2_T_1 ^ Num_reg1_5[63:0];
  wire [62:0]      _input_1_1_mul_out2_T_3 =
    Num_reg1_3[62:0] & Num_reg1_4[62:0] | Num_reg1_5[62:0]
    & _input_1_1_mul_out2_T_1[62:0];
  wire [63:0]      _input_1_2_mul_out2_T_1 = Num_reg1_6[63:0] ^ Num_reg1_7[63:0];
  wire [63:0]      input_1_2_mul_out1 = _input_1_2_mul_out2_T_1 ^ Num_reg1_8[63:0];
  wire [62:0]      _input_1_2_mul_out2_T_3 =
    Num_reg1_6[62:0] & Num_reg1_7[62:0] | Num_reg1_8[62:0]
    & _input_1_2_mul_out2_T_1[62:0];
  wire [63:0]      _input_1_3_mul_out2_T_1 = Num_reg1_9[63:0] ^ Num_reg1_10[63:0];
  wire [63:0]      input_1_3_mul_out1 = _input_1_3_mul_out2_T_1 ^ Num_reg1_11[63:0];
  wire [62:0]      _input_1_3_mul_out2_T_3 =
    Num_reg1_9[62:0] & Num_reg1_10[62:0] | Num_reg1_11[62:0]
    & _input_1_3_mul_out2_T_1[62:0];
  wire [63:0]      _input_1_4_mul_out2_T_1 = Num_reg1_12[63:0] ^ Num_reg1_13[63:0];
  wire [63:0]      input_1_4_mul_out1 = _input_1_4_mul_out2_T_1 ^ Num_reg1_14[63:0];
  wire [62:0]      _input_1_4_mul_out2_T_3 =
    Num_reg1_12[62:0] & Num_reg1_13[62:0] | Num_reg1_14[62:0]
    & _input_1_4_mul_out2_T_1[62:0];
  wire [63:0]      _input_1_5_mul_out2_T_1 = Num_reg1_15[63:0] ^ Num_reg1_16[63:0];
  wire [63:0]      input_1_5_mul_out1 = _input_1_5_mul_out2_T_1 ^ Num_reg1_17[63:0];
  wire [62:0]      _input_1_5_mul_out2_T_3 =
    Num_reg1_15[62:0] & Num_reg1_16[62:0] | Num_reg1_17[62:0]
    & _input_1_5_mul_out2_T_1[62:0];
  wire [63:0]      _input_1_6_mul_out2_T_1 = Num_reg1_18[63:0] ^ Num_reg1_19[63:0];
  wire [63:0]      input_1_6_mul_out1 = _input_1_6_mul_out2_T_1 ^ Num_reg1_20[63:0];
  wire [62:0]      _input_1_6_mul_out2_T_3 =
    Num_reg1_18[62:0] & Num_reg1_19[62:0] | Num_reg1_20[62:0]
    & _input_1_6_mul_out2_T_1[62:0];
  wire [63:0]      _input_1_7_mul_out2_T_1 = Num_reg1_21[63:0] ^ Num_reg1_22[63:0];
  wire [63:0]      input_1_7_mul_out1 = _input_1_7_mul_out2_T_1 ^ Num_reg1_23[63:0];
  wire [62:0]      _input_1_7_mul_out2_T_3 =
    Num_reg1_21[62:0] & Num_reg1_22[62:0] | Num_reg1_23[62:0]
    & _input_1_7_mul_out2_T_1[62:0];
  wire [63:0]      _input_1_8_mul_out2_T_1 = Num_reg1_24[63:0] ^ Num_reg1_25[63:0];
  wire [63:0]      input_1_8_mul_out1 = _input_1_8_mul_out2_T_1 ^ Num_reg1_26[63:0];
  wire [62:0]      _input_1_8_mul_out2_T_3 =
    Num_reg1_24[62:0] & Num_reg1_25[62:0] | Num_reg1_26[62:0]
    & _input_1_8_mul_out2_T_1[62:0];
  wire [63:0]      _input_1_9_mul_out2_T_1 = Num_reg1_27[63:0] ^ Num_reg1_28[63:0];
  wire [63:0]      input_1_9_mul_out1 = _input_1_9_mul_out2_T_1 ^ Num_reg1_29[63:0];
  wire [62:0]      _input_1_9_mul_out2_T_3 =
    Num_reg1_27[62:0] & Num_reg1_28[62:0] | Num_reg1_29[62:0]
    & _input_1_9_mul_out2_T_1[62:0];
  wire [63:0]      _input_1_10_mul_out2_T_1 = Num_reg1_30[63:0] ^ Num_reg1_31[63:0];
  wire [63:0]      input_1_10_mul_out1 = _input_1_10_mul_out2_T_1 ^ Num_reg1_32[63:0];
  wire [62:0]      _input_1_10_mul_out2_T_3 =
    Num_reg1_30[62:0] & Num_reg1_31[62:0] | Num_reg1_32[62:0]
    & _input_1_10_mul_out2_T_1[62:0];
  wire [63:0]      _input_2_0_mul_out2_T_1 =
    input_1_0_mul_out1 ^ {_input_1_0_mul_out2_T_3, 1'h0};
  wire [63:0]      input_2_0_mul_out1 = _input_2_0_mul_out2_T_1 ^ input_1_1_mul_out1;
  wire [62:0]      _input_2_0_mul_out2_T_3 =
    input_1_0_mul_out1[62:0] & {_input_1_0_mul_out2_T_3[61:0], 1'h0}
    | input_1_1_mul_out1[62:0] & _input_2_0_mul_out2_T_1[62:0];
  wire [63:0]      _input_2_1_mul_out2_T_1 =
    {_input_1_1_mul_out2_T_3, 1'h0} ^ input_1_2_mul_out1;
  wire [63:0]      input_2_1_mul_out1 =
    _input_2_1_mul_out2_T_1 ^ {_input_1_2_mul_out2_T_3, 1'h0};
  wire [62:0]      _input_2_1_mul_out2_T_3 =
    {_input_1_1_mul_out2_T_3[61:0], 1'h0} & input_1_2_mul_out1[62:0]
    | {_input_1_2_mul_out2_T_3[61:0], 1'h0} & _input_2_1_mul_out2_T_1[62:0];
  wire [63:0]      _input_2_2_mul_out2_T_1 =
    input_1_3_mul_out1 ^ {_input_1_3_mul_out2_T_3, 1'h0};
  wire [63:0]      input_2_2_mul_out1 = _input_2_2_mul_out2_T_1 ^ input_1_4_mul_out1;
  wire [62:0]      _input_2_2_mul_out2_T_3 =
    input_1_3_mul_out1[62:0] & {_input_1_3_mul_out2_T_3[61:0], 1'h0}
    | input_1_4_mul_out1[62:0] & _input_2_2_mul_out2_T_1[62:0];
  wire [63:0]      _input_2_3_mul_out2_T_1 =
    {_input_1_4_mul_out2_T_3, 1'h0} ^ input_1_5_mul_out1;
  wire [63:0]      input_2_3_mul_out1 =
    _input_2_3_mul_out2_T_1 ^ {_input_1_5_mul_out2_T_3, 1'h0};
  wire [62:0]      _input_2_3_mul_out2_T_3 =
    {_input_1_4_mul_out2_T_3[61:0], 1'h0} & input_1_5_mul_out1[62:0]
    | {_input_1_5_mul_out2_T_3[61:0], 1'h0} & _input_2_3_mul_out2_T_1[62:0];
  wire [63:0]      _input_2_4_mul_out2_T_1 =
    input_1_6_mul_out1 ^ {_input_1_6_mul_out2_T_3, 1'h0};
  wire [63:0]      input_2_4_mul_out1 = _input_2_4_mul_out2_T_1 ^ input_1_7_mul_out1;
  wire [62:0]      _input_2_4_mul_out2_T_3 =
    input_1_6_mul_out1[62:0] & {_input_1_6_mul_out2_T_3[61:0], 1'h0}
    | input_1_7_mul_out1[62:0] & _input_2_4_mul_out2_T_1[62:0];
  wire [63:0]      _input_2_5_mul_out2_T_1 =
    {_input_1_7_mul_out2_T_3, 1'h0} ^ input_1_8_mul_out1;
  wire [63:0]      input_2_5_mul_out1 =
    _input_2_5_mul_out2_T_1 ^ {_input_1_8_mul_out2_T_3, 1'h0};
  wire [62:0]      _input_2_5_mul_out2_T_3 =
    {_input_1_7_mul_out2_T_3[61:0], 1'h0} & input_1_8_mul_out1[62:0]
    | {_input_1_8_mul_out2_T_3[61:0], 1'h0} & _input_2_5_mul_out2_T_1[62:0];
  wire [63:0]      _input_2_6_mul_out2_T_1 =
    input_1_9_mul_out1 ^ {_input_1_9_mul_out2_T_3, 1'h0};
  wire [63:0]      input_2_6_mul_out1 = _input_2_6_mul_out2_T_1 ^ input_1_10_mul_out1;
  wire [62:0]      _input_2_6_mul_out2_T_3 =
    input_1_9_mul_out1[62:0] & {_input_1_9_mul_out2_T_3[61:0], 1'h0}
    | input_1_10_mul_out1[62:0] & _input_2_6_mul_out2_T_1[62:0];
  wire [63:0]      _input_3_0_mul_out2_T_1 =
    input_2_0_mul_out1 ^ {_input_2_0_mul_out2_T_3, 1'h0};
  wire [63:0]      input_3_0_mul_out1 = _input_3_0_mul_out2_T_1 ^ input_2_1_mul_out1;
  wire [62:0]      _input_3_0_mul_out2_T_3 =
    input_2_0_mul_out1[62:0] & {_input_2_0_mul_out2_T_3[61:0], 1'h0}
    | input_2_1_mul_out1[62:0] & _input_3_0_mul_out2_T_1[62:0];
  wire [63:0]      _input_3_1_mul_out2_T_1 =
    {_input_2_1_mul_out2_T_3, 1'h0} ^ input_2_2_mul_out1;
  wire [63:0]      input_3_1_mul_out1 =
    _input_3_1_mul_out2_T_1 ^ {_input_2_2_mul_out2_T_3, 1'h0};
  wire [62:0]      _input_3_1_mul_out2_T_3 =
    {_input_2_1_mul_out2_T_3[61:0], 1'h0} & input_2_2_mul_out1[62:0]
    | {_input_2_2_mul_out2_T_3[61:0], 1'h0} & _input_3_1_mul_out2_T_1[62:0];
  wire [63:0]      _input_3_2_mul_out2_T_1 =
    input_2_3_mul_out1 ^ {_input_2_3_mul_out2_T_3, 1'h0};
  wire [63:0]      input_3_2_mul_out1 = _input_3_2_mul_out2_T_1 ^ input_2_4_mul_out1;
  wire [62:0]      _input_3_2_mul_out2_T_3 =
    input_2_3_mul_out1[62:0] & {_input_2_3_mul_out2_T_3[61:0], 1'h0}
    | input_2_4_mul_out1[62:0] & _input_3_2_mul_out2_T_1[62:0];
  wire [63:0]      _input_3_3_mul_out2_T_1 =
    {_input_2_4_mul_out2_T_3, 1'h0} ^ input_2_5_mul_out1;
  wire [63:0]      input_3_3_mul_out1 =
    _input_3_3_mul_out2_T_1 ^ {_input_2_5_mul_out2_T_3, 1'h0};
  wire [62:0]      _input_3_3_mul_out2_T_3 =
    {_input_2_4_mul_out2_T_3[61:0], 1'h0} & input_2_5_mul_out1[62:0]
    | {_input_2_5_mul_out2_T_3[61:0], 1'h0} & _input_3_3_mul_out2_T_1[62:0];
  wire [63:0]      _input_3_4_mul_out2_T_1 =
    input_2_6_mul_out1 ^ {_input_2_6_mul_out2_T_3, 1'h0};
  wire [63:0]      input_3_4_mul_out1 =
    _input_3_4_mul_out2_T_1 ^ {_input_1_10_mul_out2_T_3, 1'h0};
  wire [62:0]      _input_3_4_mul_out2_T_3 =
    input_2_6_mul_out1[62:0] & {_input_2_6_mul_out2_T_3[61:0], 1'h0}
    | {_input_1_10_mul_out2_T_3[61:0], 1'h0} & _input_3_4_mul_out2_T_1[62:0];
  wire [63:0]      _input_4_0_mul_out2_T_1 =
    input_3_0_mul_out1 ^ {_input_3_0_mul_out2_T_3, 1'h0};
  wire [63:0]      input_4_0_mul_out1 = _input_4_0_mul_out2_T_1 ^ input_3_1_mul_out1;
  wire [62:0]      _input_4_0_mul_out2_T_3 =
    input_3_0_mul_out1[62:0] & {_input_3_0_mul_out2_T_3[61:0], 1'h0}
    | input_3_1_mul_out1[62:0] & _input_4_0_mul_out2_T_1[62:0];
  wire [63:0]      _input_4_1_mul_out2_T_1 =
    {_input_3_1_mul_out2_T_3, 1'h0} ^ input_3_2_mul_out1;
  wire [63:0]      input_4_1_mul_out1 =
    _input_4_1_mul_out2_T_1 ^ {_input_3_2_mul_out2_T_3, 1'h0};
  wire [62:0]      _input_4_1_mul_out2_T_3 =
    {_input_3_1_mul_out2_T_3[61:0], 1'h0} & input_3_2_mul_out1[62:0]
    | {_input_3_2_mul_out2_T_3[61:0], 1'h0} & _input_4_1_mul_out2_T_1[62:0];
  wire [63:0]      _input_4_2_mul_out2_T_1 =
    input_3_3_mul_out1 ^ {_input_3_3_mul_out2_T_3, 1'h0};
  wire [63:0]      input_4_2_mul_out1 = _input_4_2_mul_out2_T_1 ^ input_3_4_mul_out1;
  wire [62:0]      _input_4_2_mul_out2_T_3 =
    input_3_3_mul_out1[62:0] & {_input_3_3_mul_out2_T_3[61:0], 1'h0}
    | input_3_4_mul_out1[62:0] & _input_4_2_mul_out2_T_1[62:0];
  wire [63:0]      _input_5_0_mul_out2_T_1 =
    input_4_0_mul_out1 ^ {_input_4_0_mul_out2_T_3, 1'h0};
  wire [63:0]      input_5_0_mul_out1 = _input_5_0_mul_out2_T_1 ^ input_4_1_mul_out1;
  wire [62:0]      _input_5_0_mul_out2_T_3 =
    input_4_0_mul_out1[62:0] & {_input_4_0_mul_out2_T_3[61:0], 1'h0}
    | input_4_1_mul_out1[62:0] & _input_5_0_mul_out2_T_1[62:0];
  wire [63:0]      _input_5_1_mul_out2_T_1 =
    {_input_4_1_mul_out2_T_3, 1'h0} ^ input_4_2_mul_out1;
  wire [63:0]      input_5_1_mul_out1 =
    _input_5_1_mul_out2_T_1 ^ {_input_4_2_mul_out2_T_3, 1'h0};
  wire [62:0]      _input_5_1_mul_out2_T_3 =
    {_input_4_1_mul_out2_T_3[61:0], 1'h0} & input_4_2_mul_out1[62:0]
    | {_input_4_2_mul_out2_T_3[61:0], 1'h0} & _input_5_1_mul_out2_T_1[62:0];
  wire [63:0]      _input_6_0_mul_out2_T_1 =
    input_5_0_mul_out1 ^ {_input_5_0_mul_out2_T_3, 1'h0};
  wire [63:0]      input_6_0_mul_out1 = _input_6_0_mul_out2_T_1 ^ input_5_1_mul_out1;
  wire [62:0]      _input_6_0_mul_out2_T_3 =
    input_5_0_mul_out1[62:0] & {_input_5_0_mul_out2_T_3[61:0], 1'h0}
    | input_5_1_mul_out1[62:0] & _input_6_0_mul_out2_T_1[62:0];
  wire [63:0]      _input_7_0_mul_out2_T_1 =
    input_6_0_mul_out1 ^ {_input_6_0_mul_out2_T_3, 1'h0};
  wire [63:0]      input_7_0_mul_out1 =
    _input_7_0_mul_out2_T_1 ^ {_input_5_1_mul_out2_T_3, 1'h0};
  wire [62:0]      _input_7_0_mul_out2_T_3 =
    input_6_0_mul_out1[62:0] & {_input_6_0_mul_out2_T_3[61:0], 1'h0}
    | {_input_5_1_mul_out2_T_3[61:0], 1'h0} & _input_7_0_mul_out2_T_1[62:0];
  wire [63:0]      _input_8_0_mul_out2_T_1 =
    input_7_0_mul_out1 ^ {_input_7_0_mul_out2_T_3, 1'h0};
  always @(posedge clock) begin
    if (io_busy_6) begin
    end
    else begin
      Num_reg1_0 <= _GEN_2[io_num2[1:0]];
      Num_reg1_1 <=
        io_num2[3:1] == 3'h1 | io_num2[3:1] == 3'h2
          ? {_GEN_0[31:0], io_num1, 2'h0}
          : _GEN_5;
      Num_reg1_2 <=
        io_num2[5:3] == 3'h1 | io_num2[5:3] == 3'h2
          ? {_GEN_0[29:0], io_num1, 4'h0}
          : _GEN_8;
      Num_reg1_3 <=
        io_num2[7:5] == 3'h1 | io_num2[7:5] == 3'h2
          ? {_GEN_0[27:0], io_num1, 6'h0}
          : _GEN_11;
      Num_reg1_4 <=
        io_num2[9:7] == 3'h1 | io_num2[9:7] == 3'h2
          ? {_GEN_0[25:0], io_num1, 8'h0}
          : _GEN_14;
      Num_reg1_5 <=
        io_num2[11:9] == 3'h1 | io_num2[11:9] == 3'h2
          ? {_GEN_0[23:0], io_num1, 10'h0}
          : _GEN_17;
      Num_reg1_6 <=
        io_num2[13:11] == 3'h1 | io_num2[13:11] == 3'h2
          ? {_GEN_0[21:0], io_num1, 12'h0}
          : _GEN_20;
      Num_reg1_7 <=
        io_num2[15:13] == 3'h1 | io_num2[15:13] == 3'h2
          ? {_GEN_0[19:0], io_num1, 14'h0}
          : _GEN_23;
      Num_reg1_8 <=
        io_num2[17:15] == 3'h1 | io_num2[17:15] == 3'h2
          ? {_GEN_0[17:0], io_num1, 16'h0}
          : _GEN_26;
      Num_reg1_9 <=
        io_num2[19:17] == 3'h1 | io_num2[19:17] == 3'h2
          ? {_GEN_0[15:0], io_num1, 18'h0}
          : _GEN_29;
      Num_reg1_10 <=
        io_num2[21:19] == 3'h1 | io_num2[21:19] == 3'h2
          ? {_GEN_0[13:0], io_num1, 20'h0}
          : _GEN_32;
      Num_reg1_11 <=
        io_num2[23:21] == 3'h1 | io_num2[23:21] == 3'h2
          ? {_GEN_0[11:0], io_num1, 22'h0}
          : _GEN_35;
      Num_reg1_12 <=
        io_num2[25:23] == 3'h1 | io_num2[25:23] == 3'h2
          ? {_GEN_0[9:0], io_num1, 24'h0}
          : _GEN_38;
      Num_reg1_13 <=
        io_num2[27:25] == 3'h1 | io_num2[27:25] == 3'h2
          ? {_GEN_0[7:0], io_num1, 26'h0}
          : _GEN_41;
      Num_reg1_14 <=
        io_num2[29:27] == 3'h1 | io_num2[29:27] == 3'h2
          ? {_GEN_0[5:0], io_num1, 28'h0}
          : _GEN_44;
      Num_reg1_15 <=
        io_num2[31:29] == 3'h1 | io_num2[31:29] == 3'h2
          ? {_GEN_0[3:0], io_num1, 30'h0}
          : _GEN_47;
      Num_reg1_16 <=
        booth_16 == 3'h1 | booth_16 == 3'h2 ? {_GEN_0[1:0], io_num1, 32'h0} : _GEN_50;
      Num_reg1_17 <=
        _GEN_1[3:1] == 3'h1 | _GEN_1[3:1] == 3'h2 ? {io_num1, 34'h0} : _GEN_53;
      Num_reg1_18 <=
        _GEN_1[5:3] == 3'h1 | _GEN_1[5:3] == 3'h2 ? {io_num1[29:0], 36'h0} : _GEN_56;
      Num_reg1_19 <=
        _GEN_1[7:5] == 3'h1 | _GEN_1[7:5] == 3'h2 ? {io_num1[27:0], 38'h0} : _GEN_59;
      Num_reg1_20 <=
        _GEN_1[9:7] == 3'h1 | _GEN_1[9:7] == 3'h2 ? {io_num1[25:0], 40'h0} : _GEN_62;
      Num_reg1_21 <=
        _GEN_1[11:9] == 3'h1 | _GEN_1[11:9] == 3'h2 ? {io_num1[23:0], 42'h0} : _GEN_65;
      Num_reg1_22 <=
        _GEN_1[13:11] == 3'h1 | _GEN_1[13:11] == 3'h2 ? {io_num1[21:0], 44'h0} : _GEN_68;
      Num_reg1_23 <=
        _GEN_1[15:13] == 3'h1 | _GEN_1[15:13] == 3'h2 ? {io_num1[19:0], 46'h0} : _GEN_71;
      Num_reg1_24 <=
        _GEN_1[17:15] == 3'h1 | _GEN_1[17:15] == 3'h2 ? {io_num1[17:0], 48'h0} : _GEN_74;
      Num_reg1_25 <=
        _GEN_1[19:17] == 3'h1 | _GEN_1[19:17] == 3'h2 ? {io_num1[15:0], 50'h0} : _GEN_77;
      Num_reg1_26 <=
        _GEN_1[21:19] == 3'h1 | _GEN_1[21:19] == 3'h2 ? {io_num1[13:0], 52'h0} : _GEN_80;
      Num_reg1_27 <=
        _GEN_1[23:21] == 3'h1 | _GEN_1[23:21] == 3'h2 ? {io_num1[11:0], 54'h0} : _GEN_83;
      Num_reg1_28 <=
        _GEN_1[25:23] == 3'h1 | _GEN_1[25:23] == 3'h2 ? {io_num1[9:0], 56'h0} : _GEN_86;
      Num_reg1_29 <=
        _GEN_1[27:25] == 3'h1 | _GEN_1[27:25] == 3'h2 ? {io_num1[7:0], 58'h0} : _GEN_89;
      Num_reg1_30 <=
        _GEN_1[29:27] == 3'h1 | _GEN_1[29:27] == 3'h2 ? {io_num1[5:0], 60'h0} : _GEN_92;
      Num_reg1_31 <=
        _GEN_1[31:29] == 3'h1 | _GEN_1[31:29] == 3'h2 ? {io_num1[3:0], 62'h0} : _GEN_95;
      Num_reg1_32 <=
        _GEN_1[33:31] == 3'h1 | _GEN_1[33:31] == 3'h2 ? {io_num1[1:0], 64'h0} : _GEN_98;
    end
    if (io_busy_7) begin
    end
    else
      mul_out1 <= _input_8_0_mul_out2_T_1 ^ {_input_3_4_mul_out2_T_3, 1'h0};
    if (io_busy_8) begin
    end
    else
      mul_out2 <=
        {input_7_0_mul_out1[62:0] & {_input_7_0_mul_out2_T_3[61:0], 1'h0}
           | {_input_3_4_mul_out2_T_3[61:0], 1'h0} & _input_8_0_mul_out2_T_1[62:0],
         1'h0};
    if (io_busy_9) begin
    end
    else
      op_reg1 <= io_op;
    if (io_busy_10) begin
    end
    else
      op_reg2 <= op_reg1;
  end // always @(posedge)
  assign io_mul_out = op_reg2 == 5'h0 ? _mul_out_T[31:0] : _mul_out_T[63:32];
endmodule


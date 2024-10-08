// Generated by CIRCT firtool-1.62.0
module Arch_Rat(
  input         clock,
                reset,
                io_cmt_en_0,
                io_cmt_en_1,
  input  [5:0]  io_prd_cmt_0,
                io_prd_cmt_1,
                io_pprd_cmt_0,
                io_pprd_cmt_1,
  input         io_rd_valid_cmt_0,
                io_rd_valid_cmt_1,
  output        io_arch_rat_0,
                io_arch_rat_1,
                io_arch_rat_2,
                io_arch_rat_3,
                io_arch_rat_4,
                io_arch_rat_5,
                io_arch_rat_6,
                io_arch_rat_7,
                io_arch_rat_8,
                io_arch_rat_9,
                io_arch_rat_10,
                io_arch_rat_11,
                io_arch_rat_12,
                io_arch_rat_13,
                io_arch_rat_14,
                io_arch_rat_15,
                io_arch_rat_16,
                io_arch_rat_17,
                io_arch_rat_18,
                io_arch_rat_19,
                io_arch_rat_20,
                io_arch_rat_21,
                io_arch_rat_22,
                io_arch_rat_23,
                io_arch_rat_24,
                io_arch_rat_25,
                io_arch_rat_26,
                io_arch_rat_27,
                io_arch_rat_28,
                io_arch_rat_29,
                io_arch_rat_30,
                io_arch_rat_31,
                io_arch_rat_32,
                io_arch_rat_33,
                io_arch_rat_34,
                io_arch_rat_35,
                io_arch_rat_36,
                io_arch_rat_37,
                io_arch_rat_38,
                io_arch_rat_39,
                io_arch_rat_40,
                io_arch_rat_41,
                io_arch_rat_42,
                io_arch_rat_43,
                io_arch_rat_44,
                io_arch_rat_45,
                io_arch_rat_46,
                io_arch_rat_47,
                io_arch_rat_48,
                io_arch_rat_49,
                io_arch_rat_50,
                io_arch_rat_51,
                io_arch_rat_52,
                io_arch_rat_53,
                io_arch_rat_54,
                io_arch_rat_55,
                io_arch_rat_56,
                io_arch_rat_57,
                io_arch_rat_58,
                io_arch_rat_59,
                io_arch_rat_60,
                io_arch_rat_61,
                io_arch_rat_62,
                io_arch_rat_63,
  output [5:0]  io_head_arch,
  output [2:0]  io_top_arch,
  input  [1:0]  io_br_type_pred_cmt,
  input  [31:0] io_pc_cmt,
  input         io_pred_update_en_cmt,
  output [31:0] io_ras_arch_0,
                io_ras_arch_1,
                io_ras_arch_2,
                io_ras_arch_3,
                io_ras_arch_4,
                io_ras_arch_5,
                io_ras_arch_6,
                io_ras_arch_7
);

  reg         arat_0;
  reg         arat_1;
  reg         arat_2;
  reg         arat_3;
  reg         arat_4;
  reg         arat_5;
  reg         arat_6;
  reg         arat_7;
  reg         arat_8;
  reg         arat_9;
  reg         arat_10;
  reg         arat_11;
  reg         arat_12;
  reg         arat_13;
  reg         arat_14;
  reg         arat_15;
  reg         arat_16;
  reg         arat_17;
  reg         arat_18;
  reg         arat_19;
  reg         arat_20;
  reg         arat_21;
  reg         arat_22;
  reg         arat_23;
  reg         arat_24;
  reg         arat_25;
  reg         arat_26;
  reg         arat_27;
  reg         arat_28;
  reg         arat_29;
  reg         arat_30;
  reg         arat_31;
  reg         arat_32;
  reg         arat_33;
  reg         arat_34;
  reg         arat_35;
  reg         arat_36;
  reg         arat_37;
  reg         arat_38;
  reg         arat_39;
  reg         arat_40;
  reg         arat_41;
  reg         arat_42;
  reg         arat_43;
  reg         arat_44;
  reg         arat_45;
  reg         arat_46;
  reg         arat_47;
  reg         arat_48;
  reg         arat_49;
  reg         arat_50;
  reg         arat_51;
  reg         arat_52;
  reg         arat_53;
  reg         arat_54;
  reg         arat_55;
  reg         arat_56;
  reg         arat_57;
  reg         arat_58;
  reg         arat_59;
  reg         arat_60;
  reg         arat_61;
  reg         arat_62;
  reg         arat_63;
  reg  [5:0]  head;
  reg  [2:0]  top;
  reg  [31:0] ras_0;
  reg  [31:0] ras_1;
  reg  [31:0] ras_2;
  reg  [31:0] ras_3;
  reg  [31:0] ras_4;
  reg  [31:0] ras_5;
  reg  [31:0] ras_6;
  reg  [31:0] ras_7;
  wire        stack_pop = io_br_type_pred_cmt == 2'h1 & io_pred_update_en_cmt;
  wire        stack_push = io_br_type_pred_cmt[1] & io_pred_update_en_cmt;
  wire [2:0]  _top_next_T = 3'(top - 3'h1);
  wire [2:0]  _top_next_T_2 = 3'(top + 3'h1);
  wire [2:0]  _GEN = stack_push ? _top_next_T_2 : top;
  wire        _GEN_0 = io_cmt_en_0 & io_rd_valid_cmt_0;
  wire [5:0]  _GEN_1 = 6'(head + 6'h1);
  wire        _GEN_2 = io_rd_valid_cmt_0 & io_cmt_en_0;
  wire        _GEN_3 = io_prd_cmt_0 == 6'h0 | (|io_pprd_cmt_0) & arat_0;
  wire        _GEN_4 = io_prd_cmt_0 == 6'h1 | io_pprd_cmt_0 != 6'h1 & arat_1;
  wire        _GEN_5 = io_prd_cmt_0 == 6'h2 | io_pprd_cmt_0 != 6'h2 & arat_2;
  wire        _GEN_6 = io_prd_cmt_0 == 6'h3 | io_pprd_cmt_0 != 6'h3 & arat_3;
  wire        _GEN_7 = io_prd_cmt_0 == 6'h4 | io_pprd_cmt_0 != 6'h4 & arat_4;
  wire        _GEN_8 = io_prd_cmt_0 == 6'h5 | io_pprd_cmt_0 != 6'h5 & arat_5;
  wire        _GEN_9 = io_prd_cmt_0 == 6'h6 | io_pprd_cmt_0 != 6'h6 & arat_6;
  wire        _GEN_10 = io_prd_cmt_0 == 6'h7 | io_pprd_cmt_0 != 6'h7 & arat_7;
  wire        _GEN_11 = io_prd_cmt_0 == 6'h8 | io_pprd_cmt_0 != 6'h8 & arat_8;
  wire        _GEN_12 = io_prd_cmt_0 == 6'h9 | io_pprd_cmt_0 != 6'h9 & arat_9;
  wire        _GEN_13 = io_prd_cmt_0 == 6'hA | io_pprd_cmt_0 != 6'hA & arat_10;
  wire        _GEN_14 = io_prd_cmt_0 == 6'hB | io_pprd_cmt_0 != 6'hB & arat_11;
  wire        _GEN_15 = io_prd_cmt_0 == 6'hC | io_pprd_cmt_0 != 6'hC & arat_12;
  wire        _GEN_16 = io_prd_cmt_0 == 6'hD | io_pprd_cmt_0 != 6'hD & arat_13;
  wire        _GEN_17 = io_prd_cmt_0 == 6'hE | io_pprd_cmt_0 != 6'hE & arat_14;
  wire        _GEN_18 = io_prd_cmt_0 == 6'hF | io_pprd_cmt_0 != 6'hF & arat_15;
  wire        _GEN_19 = io_prd_cmt_0 == 6'h10 | io_pprd_cmt_0 != 6'h10 & arat_16;
  wire        _GEN_20 = io_prd_cmt_0 == 6'h11 | io_pprd_cmt_0 != 6'h11 & arat_17;
  wire        _GEN_21 = io_prd_cmt_0 == 6'h12 | io_pprd_cmt_0 != 6'h12 & arat_18;
  wire        _GEN_22 = io_prd_cmt_0 == 6'h13 | io_pprd_cmt_0 != 6'h13 & arat_19;
  wire        _GEN_23 = io_prd_cmt_0 == 6'h14 | io_pprd_cmt_0 != 6'h14 & arat_20;
  wire        _GEN_24 = io_prd_cmt_0 == 6'h15 | io_pprd_cmt_0 != 6'h15 & arat_21;
  wire        _GEN_25 = io_prd_cmt_0 == 6'h16 | io_pprd_cmt_0 != 6'h16 & arat_22;
  wire        _GEN_26 = io_prd_cmt_0 == 6'h17 | io_pprd_cmt_0 != 6'h17 & arat_23;
  wire        _GEN_27 = io_prd_cmt_0 == 6'h18 | io_pprd_cmt_0 != 6'h18 & arat_24;
  wire        _GEN_28 = io_prd_cmt_0 == 6'h19 | io_pprd_cmt_0 != 6'h19 & arat_25;
  wire        _GEN_29 = io_prd_cmt_0 == 6'h1A | io_pprd_cmt_0 != 6'h1A & arat_26;
  wire        _GEN_30 = io_prd_cmt_0 == 6'h1B | io_pprd_cmt_0 != 6'h1B & arat_27;
  wire        _GEN_31 = io_prd_cmt_0 == 6'h1C | io_pprd_cmt_0 != 6'h1C & arat_28;
  wire        _GEN_32 = io_prd_cmt_0 == 6'h1D | io_pprd_cmt_0 != 6'h1D & arat_29;
  wire        _GEN_33 = io_prd_cmt_0 == 6'h1E | io_pprd_cmt_0 != 6'h1E & arat_30;
  wire        _GEN_34 = io_prd_cmt_0 == 6'h1F | io_pprd_cmt_0 != 6'h1F & arat_31;
  wire        _GEN_35 = io_prd_cmt_0 == 6'h20 | io_pprd_cmt_0 != 6'h20 & arat_32;
  wire        _GEN_36 = io_prd_cmt_0 == 6'h21 | io_pprd_cmt_0 != 6'h21 & arat_33;
  wire        _GEN_37 = io_prd_cmt_0 == 6'h22 | io_pprd_cmt_0 != 6'h22 & arat_34;
  wire        _GEN_38 = io_prd_cmt_0 == 6'h23 | io_pprd_cmt_0 != 6'h23 & arat_35;
  wire        _GEN_39 = io_prd_cmt_0 == 6'h24 | io_pprd_cmt_0 != 6'h24 & arat_36;
  wire        _GEN_40 = io_prd_cmt_0 == 6'h25 | io_pprd_cmt_0 != 6'h25 & arat_37;
  wire        _GEN_41 = io_prd_cmt_0 == 6'h26 | io_pprd_cmt_0 != 6'h26 & arat_38;
  wire        _GEN_42 = io_prd_cmt_0 == 6'h27 | io_pprd_cmt_0 != 6'h27 & arat_39;
  wire        _GEN_43 = io_prd_cmt_0 == 6'h28 | io_pprd_cmt_0 != 6'h28 & arat_40;
  wire        _GEN_44 = io_prd_cmt_0 == 6'h29 | io_pprd_cmt_0 != 6'h29 & arat_41;
  wire        _GEN_45 = io_prd_cmt_0 == 6'h2A | io_pprd_cmt_0 != 6'h2A & arat_42;
  wire        _GEN_46 = io_prd_cmt_0 == 6'h2B | io_pprd_cmt_0 != 6'h2B & arat_43;
  wire        _GEN_47 = io_prd_cmt_0 == 6'h2C | io_pprd_cmt_0 != 6'h2C & arat_44;
  wire        _GEN_48 = io_prd_cmt_0 == 6'h2D | io_pprd_cmt_0 != 6'h2D & arat_45;
  wire        _GEN_49 = io_prd_cmt_0 == 6'h2E | io_pprd_cmt_0 != 6'h2E & arat_46;
  wire        _GEN_50 = io_prd_cmt_0 == 6'h2F | io_pprd_cmt_0 != 6'h2F & arat_47;
  wire        _GEN_51 = io_prd_cmt_0 == 6'h30 | io_pprd_cmt_0 != 6'h30 & arat_48;
  wire        _GEN_52 = io_prd_cmt_0 == 6'h31 | io_pprd_cmt_0 != 6'h31 & arat_49;
  wire        _GEN_53 = io_prd_cmt_0 == 6'h32 | io_pprd_cmt_0 != 6'h32 & arat_50;
  wire        _GEN_54 = io_prd_cmt_0 == 6'h33 | io_pprd_cmt_0 != 6'h33 & arat_51;
  wire        _GEN_55 = io_prd_cmt_0 == 6'h34 | io_pprd_cmt_0 != 6'h34 & arat_52;
  wire        _GEN_56 = io_prd_cmt_0 == 6'h35 | io_pprd_cmt_0 != 6'h35 & arat_53;
  wire        _GEN_57 = io_prd_cmt_0 == 6'h36 | io_pprd_cmt_0 != 6'h36 & arat_54;
  wire        _GEN_58 = io_prd_cmt_0 == 6'h37 | io_pprd_cmt_0 != 6'h37 & arat_55;
  wire        _GEN_59 = io_prd_cmt_0 == 6'h38 | io_pprd_cmt_0 != 6'h38 & arat_56;
  wire        _GEN_60 = io_prd_cmt_0 == 6'h39 | io_pprd_cmt_0 != 6'h39 & arat_57;
  wire        _GEN_61 = io_prd_cmt_0 == 6'h3A | io_pprd_cmt_0 != 6'h3A & arat_58;
  wire        _GEN_62 = io_prd_cmt_0 == 6'h3B | io_pprd_cmt_0 != 6'h3B & arat_59;
  wire        _GEN_63 = io_prd_cmt_0 == 6'h3C | io_pprd_cmt_0 != 6'h3C & arat_60;
  wire        _GEN_64 = io_prd_cmt_0 == 6'h3D | io_pprd_cmt_0 != 6'h3D & arat_61;
  wire        _GEN_65 = io_prd_cmt_0 == 6'h3E | io_pprd_cmt_0 != 6'h3E & arat_62;
  wire        _GEN_66 = (&io_prd_cmt_0) | io_pprd_cmt_0 != 6'h3F & arat_63;
  wire        _GEN_67 = _GEN_2 ? _GEN_3 : arat_0;
  wire        _GEN_68 = _GEN_2 ? _GEN_4 : arat_1;
  wire        _GEN_69 = _GEN_2 ? _GEN_5 : arat_2;
  wire        _GEN_70 = _GEN_2 ? _GEN_6 : arat_3;
  wire        _GEN_71 = _GEN_2 ? _GEN_7 : arat_4;
  wire        _GEN_72 = _GEN_2 ? _GEN_8 : arat_5;
  wire        _GEN_73 = _GEN_2 ? _GEN_9 : arat_6;
  wire        _GEN_74 = _GEN_2 ? _GEN_10 : arat_7;
  wire        _GEN_75 = _GEN_2 ? _GEN_11 : arat_8;
  wire        _GEN_76 = _GEN_2 ? _GEN_12 : arat_9;
  wire        _GEN_77 = _GEN_2 ? _GEN_13 : arat_10;
  wire        _GEN_78 = _GEN_2 ? _GEN_14 : arat_11;
  wire        _GEN_79 = _GEN_2 ? _GEN_15 : arat_12;
  wire        _GEN_80 = _GEN_2 ? _GEN_16 : arat_13;
  wire        _GEN_81 = _GEN_2 ? _GEN_17 : arat_14;
  wire        _GEN_82 = _GEN_2 ? _GEN_18 : arat_15;
  wire        _GEN_83 = _GEN_2 ? _GEN_19 : arat_16;
  wire        _GEN_84 = _GEN_2 ? _GEN_20 : arat_17;
  wire        _GEN_85 = _GEN_2 ? _GEN_21 : arat_18;
  wire        _GEN_86 = _GEN_2 ? _GEN_22 : arat_19;
  wire        _GEN_87 = _GEN_2 ? _GEN_23 : arat_20;
  wire        _GEN_88 = _GEN_2 ? _GEN_24 : arat_21;
  wire        _GEN_89 = _GEN_2 ? _GEN_25 : arat_22;
  wire        _GEN_90 = _GEN_2 ? _GEN_26 : arat_23;
  wire        _GEN_91 = _GEN_2 ? _GEN_27 : arat_24;
  wire        _GEN_92 = _GEN_2 ? _GEN_28 : arat_25;
  wire        _GEN_93 = _GEN_2 ? _GEN_29 : arat_26;
  wire        _GEN_94 = _GEN_2 ? _GEN_30 : arat_27;
  wire        _GEN_95 = _GEN_2 ? _GEN_31 : arat_28;
  wire        _GEN_96 = _GEN_2 ? _GEN_32 : arat_29;
  wire        _GEN_97 = _GEN_2 ? _GEN_33 : arat_30;
  wire        _GEN_98 = _GEN_2 ? _GEN_34 : arat_31;
  wire        _GEN_99 = _GEN_2 ? _GEN_35 : arat_32;
  wire        _GEN_100 = _GEN_2 ? _GEN_36 : arat_33;
  wire        _GEN_101 = _GEN_2 ? _GEN_37 : arat_34;
  wire        _GEN_102 = _GEN_2 ? _GEN_38 : arat_35;
  wire        _GEN_103 = _GEN_2 ? _GEN_39 : arat_36;
  wire        _GEN_104 = _GEN_2 ? _GEN_40 : arat_37;
  wire        _GEN_105 = _GEN_2 ? _GEN_41 : arat_38;
  wire        _GEN_106 = _GEN_2 ? _GEN_42 : arat_39;
  wire        _GEN_107 = _GEN_2 ? _GEN_43 : arat_40;
  wire        _GEN_108 = _GEN_2 ? _GEN_44 : arat_41;
  wire        _GEN_109 = _GEN_2 ? _GEN_45 : arat_42;
  wire        _GEN_110 = _GEN_2 ? _GEN_46 : arat_43;
  wire        _GEN_111 = _GEN_2 ? _GEN_47 : arat_44;
  wire        _GEN_112 = _GEN_2 ? _GEN_48 : arat_45;
  wire        _GEN_113 = _GEN_2 ? _GEN_49 : arat_46;
  wire        _GEN_114 = _GEN_2 ? _GEN_50 : arat_47;
  wire        _GEN_115 = _GEN_2 ? _GEN_51 : arat_48;
  wire        _GEN_116 = _GEN_2 ? _GEN_52 : arat_49;
  wire        _GEN_117 = _GEN_2 ? _GEN_53 : arat_50;
  wire        _GEN_118 = _GEN_2 ? _GEN_54 : arat_51;
  wire        _GEN_119 = _GEN_2 ? _GEN_55 : arat_52;
  wire        _GEN_120 = _GEN_2 ? _GEN_56 : arat_53;
  wire        _GEN_121 = _GEN_2 ? _GEN_57 : arat_54;
  wire        _GEN_122 = _GEN_2 ? _GEN_58 : arat_55;
  wire        _GEN_123 = _GEN_2 ? _GEN_59 : arat_56;
  wire        _GEN_124 = _GEN_2 ? _GEN_60 : arat_57;
  wire        _GEN_125 = _GEN_2 ? _GEN_61 : arat_58;
  wire        _GEN_126 = _GEN_2 ? _GEN_62 : arat_59;
  wire        _GEN_127 = _GEN_2 ? _GEN_63 : arat_60;
  wire        _GEN_128 = _GEN_2 ? _GEN_64 : arat_61;
  wire        _GEN_129 = _GEN_2 ? _GEN_65 : arat_62;
  wire        _GEN_130 = _GEN_2 ? _GEN_66 : arat_63;
  wire [5:0]  _GEN_131 = (&head) ? 6'h0 : _GEN_1;
  wire [5:0]  _GEN_132 = _GEN_0 ? _GEN_131 : head;
  always @(posedge clock) begin
    if (reset) begin
      arat_0 <= 1'h0;
      arat_1 <= 1'h0;
      arat_2 <= 1'h0;
      arat_3 <= 1'h0;
      arat_4 <= 1'h0;
      arat_5 <= 1'h0;
      arat_6 <= 1'h0;
      arat_7 <= 1'h0;
      arat_8 <= 1'h0;
      arat_9 <= 1'h0;
      arat_10 <= 1'h0;
      arat_11 <= 1'h0;
      arat_12 <= 1'h0;
      arat_13 <= 1'h0;
      arat_14 <= 1'h0;
      arat_15 <= 1'h0;
      arat_16 <= 1'h0;
      arat_17 <= 1'h0;
      arat_18 <= 1'h0;
      arat_19 <= 1'h0;
      arat_20 <= 1'h0;
      arat_21 <= 1'h0;
      arat_22 <= 1'h0;
      arat_23 <= 1'h0;
      arat_24 <= 1'h0;
      arat_25 <= 1'h0;
      arat_26 <= 1'h0;
      arat_27 <= 1'h0;
      arat_28 <= 1'h0;
      arat_29 <= 1'h0;
      arat_30 <= 1'h0;
      arat_31 <= 1'h0;
      arat_32 <= 1'h0;
      arat_33 <= 1'h0;
      arat_34 <= 1'h0;
      arat_35 <= 1'h0;
      arat_36 <= 1'h0;
      arat_37 <= 1'h0;
      arat_38 <= 1'h0;
      arat_39 <= 1'h0;
      arat_40 <= 1'h0;
      arat_41 <= 1'h0;
      arat_42 <= 1'h0;
      arat_43 <= 1'h0;
      arat_44 <= 1'h0;
      arat_45 <= 1'h0;
      arat_46 <= 1'h0;
      arat_47 <= 1'h0;
      arat_48 <= 1'h0;
      arat_49 <= 1'h0;
      arat_50 <= 1'h0;
      arat_51 <= 1'h0;
      arat_52 <= 1'h0;
      arat_53 <= 1'h0;
      arat_54 <= 1'h0;
      arat_55 <= 1'h0;
      arat_56 <= 1'h0;
      arat_57 <= 1'h0;
      arat_58 <= 1'h0;
      arat_59 <= 1'h0;
      arat_60 <= 1'h0;
      arat_61 <= 1'h0;
      arat_62 <= 1'h0;
      arat_63 <= 1'h0;
      head <= 6'h0;
      top <= 3'h7;
      ras_0 <= 32'h1C000000;
      ras_1 <= 32'h1C000000;
      ras_2 <= 32'h1C000000;
      ras_3 <= 32'h1C000000;
      ras_4 <= 32'h1C000000;
      ras_5 <= 32'h1C000000;
      ras_6 <= 32'h1C000000;
      ras_7 <= 32'h1C000000;
    end
    else begin
      if (io_rd_valid_cmt_1 & io_cmt_en_1) begin
        arat_0 <= io_prd_cmt_1 == 6'h0 | (|io_pprd_cmt_1) & _GEN_67;
        arat_1 <= io_prd_cmt_1 == 6'h1 | io_pprd_cmt_1 != 6'h1 & _GEN_68;
        arat_2 <= io_prd_cmt_1 == 6'h2 | io_pprd_cmt_1 != 6'h2 & _GEN_69;
        arat_3 <= io_prd_cmt_1 == 6'h3 | io_pprd_cmt_1 != 6'h3 & _GEN_70;
        arat_4 <= io_prd_cmt_1 == 6'h4 | io_pprd_cmt_1 != 6'h4 & _GEN_71;
        arat_5 <= io_prd_cmt_1 == 6'h5 | io_pprd_cmt_1 != 6'h5 & _GEN_72;
        arat_6 <= io_prd_cmt_1 == 6'h6 | io_pprd_cmt_1 != 6'h6 & _GEN_73;
        arat_7 <= io_prd_cmt_1 == 6'h7 | io_pprd_cmt_1 != 6'h7 & _GEN_74;
        arat_8 <= io_prd_cmt_1 == 6'h8 | io_pprd_cmt_1 != 6'h8 & _GEN_75;
        arat_9 <= io_prd_cmt_1 == 6'h9 | io_pprd_cmt_1 != 6'h9 & _GEN_76;
        arat_10 <= io_prd_cmt_1 == 6'hA | io_pprd_cmt_1 != 6'hA & _GEN_77;
        arat_11 <= io_prd_cmt_1 == 6'hB | io_pprd_cmt_1 != 6'hB & _GEN_78;
        arat_12 <= io_prd_cmt_1 == 6'hC | io_pprd_cmt_1 != 6'hC & _GEN_79;
        arat_13 <= io_prd_cmt_1 == 6'hD | io_pprd_cmt_1 != 6'hD & _GEN_80;
        arat_14 <= io_prd_cmt_1 == 6'hE | io_pprd_cmt_1 != 6'hE & _GEN_81;
        arat_15 <= io_prd_cmt_1 == 6'hF | io_pprd_cmt_1 != 6'hF & _GEN_82;
        arat_16 <= io_prd_cmt_1 == 6'h10 | io_pprd_cmt_1 != 6'h10 & _GEN_83;
        arat_17 <= io_prd_cmt_1 == 6'h11 | io_pprd_cmt_1 != 6'h11 & _GEN_84;
        arat_18 <= io_prd_cmt_1 == 6'h12 | io_pprd_cmt_1 != 6'h12 & _GEN_85;
        arat_19 <= io_prd_cmt_1 == 6'h13 | io_pprd_cmt_1 != 6'h13 & _GEN_86;
        arat_20 <= io_prd_cmt_1 == 6'h14 | io_pprd_cmt_1 != 6'h14 & _GEN_87;
        arat_21 <= io_prd_cmt_1 == 6'h15 | io_pprd_cmt_1 != 6'h15 & _GEN_88;
        arat_22 <= io_prd_cmt_1 == 6'h16 | io_pprd_cmt_1 != 6'h16 & _GEN_89;
        arat_23 <= io_prd_cmt_1 == 6'h17 | io_pprd_cmt_1 != 6'h17 & _GEN_90;
        arat_24 <= io_prd_cmt_1 == 6'h18 | io_pprd_cmt_1 != 6'h18 & _GEN_91;
        arat_25 <= io_prd_cmt_1 == 6'h19 | io_pprd_cmt_1 != 6'h19 & _GEN_92;
        arat_26 <= io_prd_cmt_1 == 6'h1A | io_pprd_cmt_1 != 6'h1A & _GEN_93;
        arat_27 <= io_prd_cmt_1 == 6'h1B | io_pprd_cmt_1 != 6'h1B & _GEN_94;
        arat_28 <= io_prd_cmt_1 == 6'h1C | io_pprd_cmt_1 != 6'h1C & _GEN_95;
        arat_29 <= io_prd_cmt_1 == 6'h1D | io_pprd_cmt_1 != 6'h1D & _GEN_96;
        arat_30 <= io_prd_cmt_1 == 6'h1E | io_pprd_cmt_1 != 6'h1E & _GEN_97;
        arat_31 <= io_prd_cmt_1 == 6'h1F | io_pprd_cmt_1 != 6'h1F & _GEN_98;
        arat_32 <= io_prd_cmt_1 == 6'h20 | io_pprd_cmt_1 != 6'h20 & _GEN_99;
        arat_33 <= io_prd_cmt_1 == 6'h21 | io_pprd_cmt_1 != 6'h21 & _GEN_100;
        arat_34 <= io_prd_cmt_1 == 6'h22 | io_pprd_cmt_1 != 6'h22 & _GEN_101;
        arat_35 <= io_prd_cmt_1 == 6'h23 | io_pprd_cmt_1 != 6'h23 & _GEN_102;
        arat_36 <= io_prd_cmt_1 == 6'h24 | io_pprd_cmt_1 != 6'h24 & _GEN_103;
        arat_37 <= io_prd_cmt_1 == 6'h25 | io_pprd_cmt_1 != 6'h25 & _GEN_104;
        arat_38 <= io_prd_cmt_1 == 6'h26 | io_pprd_cmt_1 != 6'h26 & _GEN_105;
        arat_39 <= io_prd_cmt_1 == 6'h27 | io_pprd_cmt_1 != 6'h27 & _GEN_106;
        arat_40 <= io_prd_cmt_1 == 6'h28 | io_pprd_cmt_1 != 6'h28 & _GEN_107;
        arat_41 <= io_prd_cmt_1 == 6'h29 | io_pprd_cmt_1 != 6'h29 & _GEN_108;
        arat_42 <= io_prd_cmt_1 == 6'h2A | io_pprd_cmt_1 != 6'h2A & _GEN_109;
        arat_43 <= io_prd_cmt_1 == 6'h2B | io_pprd_cmt_1 != 6'h2B & _GEN_110;
        arat_44 <= io_prd_cmt_1 == 6'h2C | io_pprd_cmt_1 != 6'h2C & _GEN_111;
        arat_45 <= io_prd_cmt_1 == 6'h2D | io_pprd_cmt_1 != 6'h2D & _GEN_112;
        arat_46 <= io_prd_cmt_1 == 6'h2E | io_pprd_cmt_1 != 6'h2E & _GEN_113;
        arat_47 <= io_prd_cmt_1 == 6'h2F | io_pprd_cmt_1 != 6'h2F & _GEN_114;
        arat_48 <= io_prd_cmt_1 == 6'h30 | io_pprd_cmt_1 != 6'h30 & _GEN_115;
        arat_49 <= io_prd_cmt_1 == 6'h31 | io_pprd_cmt_1 != 6'h31 & _GEN_116;
        arat_50 <= io_prd_cmt_1 == 6'h32 | io_pprd_cmt_1 != 6'h32 & _GEN_117;
        arat_51 <= io_prd_cmt_1 == 6'h33 | io_pprd_cmt_1 != 6'h33 & _GEN_118;
        arat_52 <= io_prd_cmt_1 == 6'h34 | io_pprd_cmt_1 != 6'h34 & _GEN_119;
        arat_53 <= io_prd_cmt_1 == 6'h35 | io_pprd_cmt_1 != 6'h35 & _GEN_120;
        arat_54 <= io_prd_cmt_1 == 6'h36 | io_pprd_cmt_1 != 6'h36 & _GEN_121;
        arat_55 <= io_prd_cmt_1 == 6'h37 | io_pprd_cmt_1 != 6'h37 & _GEN_122;
        arat_56 <= io_prd_cmt_1 == 6'h38 | io_pprd_cmt_1 != 6'h38 & _GEN_123;
        arat_57 <= io_prd_cmt_1 == 6'h39 | io_pprd_cmt_1 != 6'h39 & _GEN_124;
        arat_58 <= io_prd_cmt_1 == 6'h3A | io_pprd_cmt_1 != 6'h3A & _GEN_125;
        arat_59 <= io_prd_cmt_1 == 6'h3B | io_pprd_cmt_1 != 6'h3B & _GEN_126;
        arat_60 <= io_prd_cmt_1 == 6'h3C | io_pprd_cmt_1 != 6'h3C & _GEN_127;
        arat_61 <= io_prd_cmt_1 == 6'h3D | io_pprd_cmt_1 != 6'h3D & _GEN_128;
        arat_62 <= io_prd_cmt_1 == 6'h3E | io_pprd_cmt_1 != 6'h3E & _GEN_129;
        arat_63 <= (&io_prd_cmt_1) | io_pprd_cmt_1 != 6'h3F & _GEN_130;
      end
      else if (_GEN_2) begin
        arat_0 <= _GEN_3;
        arat_1 <= _GEN_4;
        arat_2 <= _GEN_5;
        arat_3 <= _GEN_6;
        arat_4 <= _GEN_7;
        arat_5 <= _GEN_8;
        arat_6 <= _GEN_9;
        arat_7 <= _GEN_10;
        arat_8 <= _GEN_11;
        arat_9 <= _GEN_12;
        arat_10 <= _GEN_13;
        arat_11 <= _GEN_14;
        arat_12 <= _GEN_15;
        arat_13 <= _GEN_16;
        arat_14 <= _GEN_17;
        arat_15 <= _GEN_18;
        arat_16 <= _GEN_19;
        arat_17 <= _GEN_20;
        arat_18 <= _GEN_21;
        arat_19 <= _GEN_22;
        arat_20 <= _GEN_23;
        arat_21 <= _GEN_24;
        arat_22 <= _GEN_25;
        arat_23 <= _GEN_26;
        arat_24 <= _GEN_27;
        arat_25 <= _GEN_28;
        arat_26 <= _GEN_29;
        arat_27 <= _GEN_30;
        arat_28 <= _GEN_31;
        arat_29 <= _GEN_32;
        arat_30 <= _GEN_33;
        arat_31 <= _GEN_34;
        arat_32 <= _GEN_35;
        arat_33 <= _GEN_36;
        arat_34 <= _GEN_37;
        arat_35 <= _GEN_38;
        arat_36 <= _GEN_39;
        arat_37 <= _GEN_40;
        arat_38 <= _GEN_41;
        arat_39 <= _GEN_42;
        arat_40 <= _GEN_43;
        arat_41 <= _GEN_44;
        arat_42 <= _GEN_45;
        arat_43 <= _GEN_46;
        arat_44 <= _GEN_47;
        arat_45 <= _GEN_48;
        arat_46 <= _GEN_49;
        arat_47 <= _GEN_50;
        arat_48 <= _GEN_51;
        arat_49 <= _GEN_52;
        arat_50 <= _GEN_53;
        arat_51 <= _GEN_54;
        arat_52 <= _GEN_55;
        arat_53 <= _GEN_56;
        arat_54 <= _GEN_57;
        arat_55 <= _GEN_58;
        arat_56 <= _GEN_59;
        arat_57 <= _GEN_60;
        arat_58 <= _GEN_61;
        arat_59 <= _GEN_62;
        arat_60 <= _GEN_63;
        arat_61 <= _GEN_64;
        arat_62 <= _GEN_65;
        arat_63 <= _GEN_66;
      end
      if (io_cmt_en_1 & io_rd_valid_cmt_1) begin
        if (&_GEN_132)
          head <= 6'h0;
        else
          head <= 6'(_GEN_132 + 6'h1);
      end
      else if (_GEN_0) begin
        if (&head)
          head <= 6'h0;
        else
          head <= _GEN_1;
      end
      if (stack_pop)
        top <= _top_next_T;
      else if (stack_push)
        top <= _top_next_T_2;
      if (stack_pop | ~(stack_push & top == 3'h0)) begin
      end
      else
        ras_0 <= io_pc_cmt;
      if (stack_pop | ~(stack_push & top == 3'h1)) begin
      end
      else
        ras_1 <= io_pc_cmt;
      if (stack_pop | ~(stack_push & top == 3'h2)) begin
      end
      else
        ras_2 <= io_pc_cmt;
      if (stack_pop | ~(stack_push & top == 3'h3)) begin
      end
      else
        ras_3 <= io_pc_cmt;
      if (stack_pop | ~(stack_push & top == 3'h4)) begin
      end
      else
        ras_4 <= io_pc_cmt;
      if (stack_pop | ~(stack_push & top == 3'h5)) begin
      end
      else
        ras_5 <= io_pc_cmt;
      if (stack_pop | ~(stack_push & top == 3'h6)) begin
      end
      else
        ras_6 <= io_pc_cmt;
      if (stack_pop | ~(stack_push & (&top))) begin
      end
      else
        ras_7 <= io_pc_cmt;
    end
  end // always @(posedge)
  assign io_arch_rat_0 = arat_0;
  assign io_arch_rat_1 = arat_1;
  assign io_arch_rat_2 = arat_2;
  assign io_arch_rat_3 = arat_3;
  assign io_arch_rat_4 = arat_4;
  assign io_arch_rat_5 = arat_5;
  assign io_arch_rat_6 = arat_6;
  assign io_arch_rat_7 = arat_7;
  assign io_arch_rat_8 = arat_8;
  assign io_arch_rat_9 = arat_9;
  assign io_arch_rat_10 = arat_10;
  assign io_arch_rat_11 = arat_11;
  assign io_arch_rat_12 = arat_12;
  assign io_arch_rat_13 = arat_13;
  assign io_arch_rat_14 = arat_14;
  assign io_arch_rat_15 = arat_15;
  assign io_arch_rat_16 = arat_16;
  assign io_arch_rat_17 = arat_17;
  assign io_arch_rat_18 = arat_18;
  assign io_arch_rat_19 = arat_19;
  assign io_arch_rat_20 = arat_20;
  assign io_arch_rat_21 = arat_21;
  assign io_arch_rat_22 = arat_22;
  assign io_arch_rat_23 = arat_23;
  assign io_arch_rat_24 = arat_24;
  assign io_arch_rat_25 = arat_25;
  assign io_arch_rat_26 = arat_26;
  assign io_arch_rat_27 = arat_27;
  assign io_arch_rat_28 = arat_28;
  assign io_arch_rat_29 = arat_29;
  assign io_arch_rat_30 = arat_30;
  assign io_arch_rat_31 = arat_31;
  assign io_arch_rat_32 = arat_32;
  assign io_arch_rat_33 = arat_33;
  assign io_arch_rat_34 = arat_34;
  assign io_arch_rat_35 = arat_35;
  assign io_arch_rat_36 = arat_36;
  assign io_arch_rat_37 = arat_37;
  assign io_arch_rat_38 = arat_38;
  assign io_arch_rat_39 = arat_39;
  assign io_arch_rat_40 = arat_40;
  assign io_arch_rat_41 = arat_41;
  assign io_arch_rat_42 = arat_42;
  assign io_arch_rat_43 = arat_43;
  assign io_arch_rat_44 = arat_44;
  assign io_arch_rat_45 = arat_45;
  assign io_arch_rat_46 = arat_46;
  assign io_arch_rat_47 = arat_47;
  assign io_arch_rat_48 = arat_48;
  assign io_arch_rat_49 = arat_49;
  assign io_arch_rat_50 = arat_50;
  assign io_arch_rat_51 = arat_51;
  assign io_arch_rat_52 = arat_52;
  assign io_arch_rat_53 = arat_53;
  assign io_arch_rat_54 = arat_54;
  assign io_arch_rat_55 = arat_55;
  assign io_arch_rat_56 = arat_56;
  assign io_arch_rat_57 = arat_57;
  assign io_arch_rat_58 = arat_58;
  assign io_arch_rat_59 = arat_59;
  assign io_arch_rat_60 = arat_60;
  assign io_arch_rat_61 = arat_61;
  assign io_arch_rat_62 = arat_62;
  assign io_arch_rat_63 = arat_63;
  assign io_head_arch = head;
  assign io_top_arch = stack_pop ? _top_next_T : _GEN;
  assign io_ras_arch_0 = ras_0;
  assign io_ras_arch_1 = ras_1;
  assign io_ras_arch_2 = ras_2;
  assign io_ras_arch_3 = ras_3;
  assign io_ras_arch_4 = ras_4;
  assign io_ras_arch_5 = ras_5;
  assign io_ras_arch_6 = ras_6;
  assign io_ras_arch_7 = ras_7;
endmodule


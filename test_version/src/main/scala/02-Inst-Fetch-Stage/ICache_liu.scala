import chisel3._
import chisel3.util._

object ICache_param{
    val INDEX_WIDTH = 7
    val INDEX_NUM = 1 << INDEX_WIDTH
    val OFFSET_WIDTH = 6
    val OFFSET_NUM = 1 << OFFSET_WIDTH
    val TAG_WIDTH = 32 - OFFSET_WIDTH - INDEX_WIDTH
    val FROM_MEM = 1.U(1.W)
    val FROM_BUF = 0.U(1.W)
    val FROM_PIPE = 1.U(1.W)
    val FROM_SEG = 0.U(1.W)
}

import ICache_param._
class ICache_IO extends Bundle{
    // IF Stage
    val addr_IF         = Input(UInt(32.W))
    val paddr_IF        = Input(UInt(32.W))
    val rvalid_IF       = Input(Bool())
    val uncache_IF      = Input(Bool())
    val cacop_en        = Input(Bool())
    val cacop_op        = Input(UInt(2.W))
    val has_cacop_IF    = Output(Bool())

    // RM Stage
    val cache_miss_RM   = Output(Bool())
    val rdata_RM        = Output(Vec(2, UInt(32.W)))
    val exception_RM    = Input(Bool())

    // control
    val stall           = Input(Bool())
    val flush           = Input(Bool())

    // for AXI arbiter
    val i_araddr        = Output(UInt(32.W))
    val i_rvalid        = Output(Bool())
    val i_rready        = Input(Bool())
    val i_rdata         = Input(UInt(32.W))
    val i_rlast         = Input(Bool())
    val i_rsize         = Output(UInt(3.W))
    val i_rburst        = Output(UInt(2.W))
    val i_rlen          = Output(UInt(8.W))

    // for stat
    val commit_icache_visit    = Output(Bool())
    val commit_icache_miss     = Output(Bool())

    // useless
    val pc_sign_RM      = Output(Vec(2, UInt(2.W)))
}

//class ICache_IO extends Bundle{
//    // IF Stage
//    val addr_IF         = Input(UInt(32.W))
//    val paddr_IF        = Input(UInt(32.W))
//    val rvalid_IF       = Input(Bool())
//    val uncache_IF      = Input(Bool())
//    val has_cacop_IF    = Output(Bool())
//
//    // RM Stage
//    val cache_miss_RM   = Output(Bool())
//    val rdata_RM        = Output(Vec(2, UInt(32.W)))
//    val pc_sign_RM      = Output(Vec(2, UInt(2.W)))
//    val exception_RM    = Input(Bool())
//
//    // control
//    val stall           = Input(Bool())
//    val flush           = Input(Bool())
//
//    // for AXI arbiter
//    val i_araddr        = Output(UInt(32.W))
//    val i_rvalid        = Output(Bool())
//    val i_rready        = Input(Bool())
//    val i_rdata         = Input(UInt(32.W))
//    val i_rlast         = Input(Bool())
//    val i_rsize         = Output(UInt(3.W))
//    val i_rburst        = Output(UInt(2.W))
//    val i_rlen          = Output(UInt(8.W))
//
//    // cacop 
//    val cacop_en      = Input(Bool())
//    val cacop_op      = Input(UInt(2.W))
//
//    // for stat
//    val commit_icache_visit    = Output(Bool())
//    val commit_icache_miss     = Output(Bool())
//}

class ICache extends Module{
    val io = IO(new ICache_IO)
    // no_use
    //io.has_cacop_IF := false.B
    io.pc_sign_RM := VecInit.fill(2)(0.U(2.W))
    
    // input
    val vaddr_IF = io.addr_IF
    val paddr_IF = io.paddr_IF
    val rvalid_IF = io.rvalid_IF
    val exception = io.exception_RM
    val stall = io.stall
    val flush = io.flush

    // memory
    val tagmem = VecInit.fill(2)(Module(new xilinx_single_port_ram_no_change(TAG_WIDTH+1, INDEX_NUM)).io)
    val instmem = VecInit.fill(2)(Module(new xilinx_single_port_ram_no_change(8 * OFFSET_NUM, INDEX_NUM)).io)

    // IF stage
    val index_IF = vaddr_IF(INDEX_WIDTH+OFFSET_WIDTH-1, OFFSET_WIDTH)
    val tagr = VecInit.tabulate(2)(i => tagmem(i).douta(TAG_WIDTH-1, 0))
    val validr = VecInit.tabulate(2)(i => tagmem(i).douta(TAG_WIDTH))
    val addr_sel = WireDefault(FROM_PIPE)

    val cacop_en_IF = RegInit(false.B)
    val cacop_op_IF = RegInit(0.U(2.W))
    val cacop_addr_IF = RegInit(0.U(32.W))


    // segreg
    val vaddr_IF_RM = RegInit(0.U(32.W))
    val paddr_IF_RM = RegInit(0.U(32.W))
    val rvalid_IF_RM = RegInit(false.B)
    val uncache_IF_RM = RegInit(false.B)
    val cacop_en_IF_RM = RegInit(false.B)
    val cacop_op_IF_RM = RegInit(0.U(2.W))

    // retbuf
    val rbuf = RegInit(0.U((8*OFFSET_NUM).W))

    // RM stage
    val vaddr_RM = vaddr_IF_RM
    val paddr_RM = paddr_IF_RM
    val rvalid_RM = rvalid_IF_RM
    val tag_RM = paddr_RM(31, INDEX_WIDTH+OFFSET_WIDTH)
    val index_RM = paddr_RM(OFFSET_WIDTH+INDEX_WIDTH-1, OFFSET_WIDTH)
    val offset_RM = paddr_RM(OFFSET_WIDTH-1, 0)
    val hit = VecInit.tabulate(2)(i => validr(i) && !(tagr(i) ^ paddr_RM(31, INDEX_WIDTH+OFFSET_WIDTH)))
    val hit_idx = OHToUInt(hit)
    val cache_hit_RM = hit.asUInt.orR
    val cache_miss_RM = WireDefault(0.U(1.W))
    val tag_we = WireDefault(VecInit.fill(2)(false.B))
    val inst_we = WireDefault(VecInit.fill(2)(false.B))
    val data_sel = WireDefault(FROM_BUF)
    val uncache_RM = uncache_IF_RM
    val cacop_en_RM = cacop_en_IF_RM
    val cacop_op_RM = cacop_op_IF_RM

    //lru
    val lrumem = RegInit(VecInit.fill(INDEX_NUM)(0.U(1.W)))
    val lru_sel = lrumem(index_RM)
    val lru_miss_upd = WireDefault(false.B)
    val lru_hit_upd = WireDefault(false.B)

    // segreg logic
    when(!(stall || cache_miss_RM.orR)){
        vaddr_IF_RM := vaddr_IF
        paddr_IF_RM := paddr_IF
        rvalid_IF_RM := rvalid_IF
        uncache_IF_RM := io.uncache_IF
        cacop_en_IF_RM := cacop_en_IF
        cacop_op_IF_RM := cacop_op_IF
    }

    when(!cacop_en_IF){
        cacop_en_IF := io.cacop_en
        cacop_op_IF := io.cacop_op
        cacop_addr_IF := io.paddr_IF
    }.elsewhen(!(stall || cache_miss_RM.orR)){
        cacop_en_IF := false.B
    }

    //mem logic
    for(i <- 0 until 2){
        tagmem(i).addra := Mux(addr_sel === FROM_PIPE, index_IF, index_RM)
        tagmem(i).dina := true.B ## tag_RM
        tagmem(i).wea := tag_we(i)
        tagmem(i).clka := clock
    }
    for(i <- 0 until 2){
        instmem(i).addra := Mux(addr_sel === FROM_PIPE, index_IF, index_RM)
        instmem(i).dina := rbuf
        instmem(i).wea := inst_we(i)
        instmem(i).clka := clock
    }

    val cacop_way_RM = Mux(cacop_op_RM(1), hit_idx, paddr_RM(0))
    val cacop_exec_RM = Mux(cacop_op_RM(1), cache_hit_RM, true.B)

    // read out
    val inst_line = Mux1H(hit, instmem.map(_.douta))
    val inst_group = VecInit.tabulate(OFFSET_NUM/4)(i => if(i == OFFSET_NUM/4-1) 0.U(32.W) ## inst_line(31+32*i, 32*i) else inst_line(63+32*i, 32*i))
    val inst_read = inst_group(offset_RM(OFFSET_WIDTH-1, 2))
    
    val ret_group = VecInit.tabulate(OFFSET_NUM/4)(i => if(i == OFFSET_NUM/4-1) 0.U(32.W) ## rbuf(31+32*i, 32*i) else rbuf(63+32*i, 32*i))
    val ret_read = Mux(uncache_RM, rbuf(8*OFFSET_NUM-1, 8*OFFSET_NUM-64), ret_group(offset_RM(OFFSET_WIDTH-1, 2)))

    val rdata = VecInit.tabulate(2)(i => Mux(data_sel === FROM_MEM, inst_read(31+32*i, 32*i), ret_read(31+32*i, 32*i)))

    // lru update logic
    when(lru_hit_upd){
        lrumem(index_RM) := !hit_idx
    }.elsewhen(lru_miss_upd){
        lrumem(index_RM) := !lru_sel
    }

    // ret_buf update logic
    when(io.i_rready){
        rbuf := io.i_rdata ## rbuf(8*OFFSET_NUM-1, 32)
    }

    val s_idle :: s_miss :: s_wait :: s_refill :: Nil = Enum(4)
    val cs = RegInit(s_idle)
    val irvalid = WireDefault(false.B)

    val icache_miss = WireDefault(false.B)
    val icache_visit = WireDefault(false.B)

    // fsm
    switch(cs) {
        is(s_idle){
            when(io.exception_RM){
                cs := s_idle
            }.elsewhen(cacop_en_RM){
                cs := Mux(cacop_exec_RM, s_refill, s_idle)
                addr_sel := Mux(cacop_exec_RM, FROM_SEG, FROM_PIPE)
                cache_miss_RM := cacop_exec_RM
            }.elsewhen(rvalid_RM){
                when(uncache_RM){
                    cs := s_miss
                    cache_miss_RM := true.B
                    addr_sel := FROM_SEG
                }.otherwise{
                    cs := Mux(cache_hit_RM, s_idle, s_miss)
                    lru_hit_upd := cache_hit_RM
                    cache_miss_RM := !cache_hit_RM
                    data_sel := FROM_MEM
                    addr_sel := Mux(stall, FROM_SEG, FROM_PIPE)
                    irvalid := cache_miss_RM
                    icache_visit := !stall
                    icache_miss := !cache_hit_RM
                }
            }
        }
        is(s_miss){
            irvalid := true.B
            cache_miss_RM := true.B
            cs := Mux(io.i_rready && io.i_rlast, Mux(uncache_RM, s_wait, s_refill), s_miss)
            addr_sel := FROM_SEG
        }
        is(s_refill){
            val tag_index = Mux(cacop_en_RM, cacop_way_RM, lru_sel)
            cs := s_wait
            cache_miss_RM := true.B
            lru_miss_upd := !cacop_en_RM
            tag_we(tag_index) := true.B
            inst_we(lru_sel) := !cacop_en_RM
            addr_sel := FROM_SEG
        }
        is(s_wait){
            cs := Mux(stall, s_wait, s_idle)
            cache_miss_RM := false.B
        }
    }

    // output
    io.cache_miss_RM := cache_miss_RM
    io.rdata_RM := rdata
    io.i_araddr := Mux(uncache_RM, paddr_RM, tag_RM ## index_RM ## 0.U(OFFSET_WIDTH.W))
    io.i_rvalid := irvalid
    io.i_rsize := 2.U
    io.i_rburst := 1.U
    io.i_rlen := Mux(uncache_RM, 1.U, (8*OFFSET_NUM/32-1).U)
    io.has_cacop_IF := cacop_en_IF

    io.commit_icache_miss := icache_miss
    io.commit_icache_visit := icache_visit
}   
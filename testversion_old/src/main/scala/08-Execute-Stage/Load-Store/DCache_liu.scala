import chisel3._
import chisel3.util._

object DCache_Config{
    val INDEX_WIDTH = 5
    val INDEX_NUM = 1 << INDEX_WIDTH

    val OFFSET_WIDTH = 5
    val OFFSET_NUM = 1 << OFFSET_WIDTH

    val TAG_WIDTH = 32 - INDEX_WIDTH - OFFSET_WIDTH

    val FROM_MEM = 0.U(1.W)
    val FROM_BUF = 1.U(1.W)

    val FROM_PIPE = 0.U(1.W)
    val FROM_SEG  = 1.U(1.W)
}

import DCache_Config._
class DCache_IO extends Bundle{
    // RF stage
    val addr_RF         = Input(UInt(32.W))
    val mem_type_RF     = Input(UInt(5.W))
    val wdata_RF        = Input(UInt(32.W))
    val store_cmt_RF    = Input(Bool()) // no use
    // val uncache_RF      = Input(Bool())

    // EX stage
    val rob_index_EX    = Input(UInt(log2Ceil(32).W)) // ROB_NUM = 24
    val paddr_EX        = Input(UInt(32.W))
    val uncache_EX      = Input(Bool())
    // val exception_EX    = Input(Bool())

    // MEM stage
    val cache_miss_MEM  = Output(Vec(5, Bool()))
    val rdata_MEM       = Output(UInt(32.W))
    val exception_MEM   = Input(Bool())

    val cache_miss_iq   = Output(Vec(5, Bool()))

    // uncache cmt
    val rob_index_CMT   = Input(UInt(log2Ceil(24).W))

    // cacop 
    val cacop_en        = Input(Bool())
    val cacop_op        = Input(UInt(2.W))

    // control
    val stall           = Input(Bool())
    val flush           = Input(Bool())
    val has_store       = Output(Bool())

    // for AXI arbiter
    val d_araddr        = Output(UInt(32.W))
    val d_rvalid        = Output(Bool())
    val d_rready        = Input(Bool())
    val d_rdata         = Input(UInt(32.W))
    val d_rlast         = Input(Bool())
    val d_rsize         = Output(UInt(3.W))
    val d_rburst        = Output(UInt(2.W))
    val d_rlen          = Output(UInt(8.W))

    val d_awaddr        = Output(UInt(32.W))
    val d_wdata         = Output(UInt(32.W))
    val d_wvalid        = Output(Bool())
    val d_wready        = Input(Bool())
    val d_wlast         = Output(Bool())
    val d_wstrb         = Output(UInt(4.W))
    val d_wsize         = Output(UInt(3.W))
    val d_wburst        = Output(UInt(2.W))
    val d_wlen          = Output(UInt(8.W))
    val d_bvalid        = Input(Bool())
    val d_bready        = Output(Bool())

    // for stat
    val commit_dcache_visit    = Output(Bool())
    val commit_dcache_miss     = Output(Bool())
}

class DCache extends Module{
    val io = IO(new DCache_IO)
    val stall = io.stall

    // RF stage
    val addr_RF = io.addr_RF
    val tag_RF = addr_RF(31, INDEX_WIDTH+OFFSET_WIDTH)
    val index_RF = addr_RF(INDEX_WIDTH+OFFSET_WIDTH-1, OFFSET_WIDTH)
    val offset_RF = addr_RF(OFFSET_WIDTH-1, 0)
    val mem_type_RF = io.mem_type_RF
    val wdata_RF = io.wdata_RF
    val store_cmt_RF = io.store_cmt_RF

    val data_sel = WireDefault(FROM_BUF)
    val addr_sel = WireDefault(FROM_PIPE)
    val d_rvalid = WireDefault(false.B)

    val cache_miss = WireDefault(VecInit.fill(5)(false.B))

    // RF-EX segreg
    val RF_EX_en = !(stall || cache_miss(4))
    val addr_RF_EX = ShiftRegister(io.addr_RF, 1, 0.U(32.W), RF_EX_en)
    val mem_type_RF_EX = ShiftRegister(io.mem_type_RF, 1, 0.U(5.W), RF_EX_en)
    val wdata_RF_EX = ShiftRegister(io.wdata_RF, 1, 0.U(32.W), RF_EX_en)
    val flush_RF_EX = ShiftRegister(io.flush, 1, false.B, RF_EX_en || io.flush)

    // banks
    val tagmem = VecInit.fill(2)(Module(new xilinx_simple_dual_port_1_clock_ram_no_change(TAG_WIDTH+1, INDEX_NUM)).io)
    val datamem = VecInit.fill(2)(Module(new xilinx_simple_dual_port_byte_write_1_clock_ram_read_first(OFFSET_NUM, 8, INDEX_NUM)).io)

    // EX stage
    val tag_r_EX = VecInit.tabulate(2)(i => tagmem(i).doutb(TAG_WIDTH-1, 0))
    val valid_r_EX = VecInit.tabulate(2)(i => tagmem(i).doutb(TAG_WIDTH))
    val data_r_EX = VecInit.tabulate(2)(i => datamem(i).doutb)
    val hit_EX = WireDefault(0.U(2.W))

    val addr_EX = addr_RF_EX
    val tag_EX = addr_EX(31, INDEX_WIDTH+OFFSET_WIDTH)
    val index_EX = addr_EX(INDEX_WIDTH+OFFSET_WIDTH-1, OFFSET_WIDTH)

    // EX_MEM segreg
    val EX_MEM_en = !(stall || cache_miss(4))
    val addr_EX_MEM = ShiftRegister(addr_EX, 1, 0.U(32.W), EX_MEM_en)
    val mem_type_EX_MEM = ShiftRegister(Mux(mem_type_RF_EX(3), mem_type_RF_EX, 0.U), 1, 0.U, EX_MEM_en)
    val wdata_EX_MEM = ShiftRegister(wdata_RF_EX, 1, 0.U, EX_MEM_en)
    val hit_EX_MEM = ShiftRegister(VecInit.fill(5)(hit_EX), 1, VecInit.fill(5)(0.U(2.W)), EX_MEM_en)
    val valid_r_EX_MEM = ShiftRegister(valid_r_EX, 1, VecInit.fill(2)(0.U), EX_MEM_en)
    val tag_r_EX_MEM = ShiftRegister(tag_r_EX, 1, VecInit.fill(2)(0.U(TAG_WIDTH.W)), EX_MEM_en)
    val flush_EX_MEM = ShiftRegister(Mux(io.flush, io.flush, flush_RF_EX), 1, false.B, EX_MEM_en || io.flush)
    val data_r_EX_MEM = ShiftRegister(data_r_EX, 1, VecInit.fill(2)(0.U((8*OFFSET_NUM).W)), EX_MEM_en)
    val mem_type_EX_MEM_backup = ShiftRegister(VecInit.fill(5)(Mux(mem_type_RF_EX(3), mem_type_RF_EX, 0.U)), 1, VecInit.fill(5)(0.U(5.W)), EX_MEM_en)

    // MEM stage
    val addr_MEM = addr_EX_MEM
    val mem_type_MEM = mem_type_EX_MEM
    val wdata_MEM = WireDefault(wdata_EX_MEM)
    val data_r_MEM = data_r_EX_MEM
    val mem_type_backup = mem_type_EX_MEM_backup

    val tag_we = WireDefault(VecInit.fill(2)(false.B))
    val data_we = WireDefault(VecInit.fill(2)(0.U(OFFSET_NUM.W)))

    val tag_MEM = addr_MEM(31, OFFSET_WIDTH+INDEX_WIDTH)
    val index_MEM = addr_MEM(INDEX_WIDTH+OFFSET_WIDTH-1, OFFSET_WIDTH)
    val offset_MEM = addr_MEM(OFFSET_WIDTH-1, 0)
    val tag_r_MEM = tag_r_EX_MEM
    val valid_r_MEM = valid_r_EX_MEM

    // lru
    val lrumem = RegInit(VecInit.fill(INDEX_NUM)(0.U(1.W)))
    val lru_sel = lrumem(index_MEM)
    val lru_miss_upd = WireDefault(false.B)
    val lru_hit_upd = WireDefault(false.B)

    val rbuf = RegInit(0.U((8*OFFSET_NUM).W))
    val rbuf_we = WireDefault(false.B)
    val wbuf = RegInit(0.U((8*OFFSET_NUM+32).W))
    val wbuf_we = WireDefault(false.B)

    // dirty table
    val dt = RegInit(VecInit.fill(2)(VecInit.fill(INDEX_NUM)(false.B)))
    val dt_we = WireDefault(false.B)
    val dt_cln = WireDefault(false.B)
    val is_dirty = dt(lru_sel)(index_MEM)

    // wfsm ctrl
    val wfsm_en = WireDefault(false.B)
    val wfsm_reset = WireDefault(false.B)
    val d_wvalid = WireDefault(false.B)
    val d_wlast = WireDefault(false.B)
    val d_bready = WireDefault(false.B)

    val d_visit = WireDefault(false.B)
    val d_miss = WireDefault(false.B)

    // EX logic
    for (i <- 0 until 2){
        tagmem(i).addra := index_MEM
        tagmem(i).addrb := Mux(addr_sel === FROM_PIPE, index_RF, index_EX)
        tagmem(i).dina := true.B ## tag_MEM
        tagmem(i).clka := clock
        tagmem(i).wea := tag_we(i)
    }

    for (i <- 0 until 2){
        datamem(i).addra := index_MEM
        datamem(i).addrb := Mux(addr_sel === FROM_PIPE, index_RF, index_EX)
        datamem(i).dina := wdata_MEM
        datamem(i).clka := clock
        datamem(i).wea := data_we(i)
    }

    hit_EX := VecInit.tabulate(2)(i => valid_r_EX(i) && !(tag_r_EX(i) ^ tag_EX)).asUInt
    val hit_MEM = VecInit.tabulate(5)(i => VecInit.tabulate(2)(j => hit_EX_MEM(i)(j)).asUInt)
    val hit_index = VecInit.tabulate(5)(i => OHToUInt(hit_MEM(i)))
    val cache_hit = VecInit.tabulate(5)(i => hit_MEM(i).orR)
    
    // read logic
    val wdoffset = offset_MEM ## 0.U(3.W)
    val data_frommem = (data_r_MEM(hit_index(0)) >> wdoffset)(31, 0)
    val data_frombuf = (rbuf >> wdoffset)(31, 0)
    val data_read = Mux(data_sel === FROM_BUF, data_frombuf, data_frommem)

    val read_bytenum = (1.U << mem_type_MEM(1, 0)) ## 0.U(3.W)
    val read_mask = (UIntToOH(read_bytenum) - 1.U)(31, 0)
    val rdata = read_mask & data_read

    // write logic
    val w_mask = Mux(mem_type_MEM(3), 0.U((8*OFFSET_NUM).W), (0.U((8*OFFSET_NUM-32).W) ## read_mask) << wdoffset)
    val refill_temp = (0.U((8*OFFSET_NUM-32).W) ## wdata_EX_MEM) << wdoffset
    wdata_MEM := (w_mask & refill_temp) | (~w_mask & rbuf)
    val wmask_byte = VecInit.tabulate(OFFSET_NUM)(i => w_mask(8*i)).asUInt

    when(io.d_rready){
        rbuf := io.d_rdata ## rbuf(8*OFFSET_NUM-1, 32)
    }

    when(lru_hit_upd){
        lrumem(index_MEM) := !hit_index(1)
    }.elsewhen(lru_miss_upd){
        lrumem(index_MEM) := !lru_sel
    }

    val write_way = Mux(cache_hit(2), hit_index(2), lru_sel)
    when(dt_we){
        dt(write_way)(index_MEM) := true.B
    }.elsewhen(dt_cln){
        dt(lru_sel)(index_MEM) := false.B
    }

    when(wbuf_we){
        wbuf := data_r_MEM(lru_sel) ## tag_r_MEM(lru_sel) ## addr_MEM(INDEX_WIDTH+OFFSET_WIDTH-1, OFFSET_WIDTH) ## 0.U(OFFSET_WIDTH.W)
    }.elsewhen(io.d_wready && io.d_wvalid){
        wbuf := 0.U(32.W) ## wbuf(8*OFFSET_NUM+32-1, 64) ## wbuf(31, 0)
    }

    val s_idle :: s_miss :: s_refill :: s_wait :: Nil = Enum(4)
    val cs = RegInit(s_idle)
    val cs_backup = RegInit(VecInit.fill(5)(s_idle))
    val w_finish = WireDefault(false.B)

    switch(cs){
        is(s_idle){
            when(mem_type_MEM(4, 3).orR){
                cs := Mux(cache_hit(0), s_idle, s_miss)
                lru_hit_upd := cache_hit(1)
                data_sel := FROM_MEM
                data_we(hit_index(2)) := Mux(mem_type_MEM(4) && cache_hit(1), wmask_byte, 0.U)
                dt_we := mem_type_MEM(4)
                wbuf_we := !cache_hit(2)
                wfsm_en := !cache_hit(3)
                addr_sel := FROM_PIPE
                d_visit := true.B
                d_miss := !cache_hit(4)
            }
        }
        is(s_miss){
            d_rvalid := true.B
            cs := Mux(io.d_rready && io.d_rlast, s_refill, s_miss)
            addr_sel := FROM_SEG
        }
        is(s_refill){
            cs := s_wait
            lru_miss_upd := true.B
            tag_we(lru_sel) := true.B
            data_we(lru_sel) := Fill(OFFSET_NUM, 1.U(1.W))
            dt_cln := mem_type_MEM(3)
            dt_we := mem_type_MEM(4)
            addr_sel := FROM_SEG
        }
        is(s_wait){
            addr_sel := Mux(w_finish, FROM_PIPE, FROM_SEG)
            cs := Mux(w_finish, s_idle, s_wait)
            wfsm_reset := true.B
        }
    }

    // optimize fanout
    for(i <- 0 until 5){
        switch(cs_backup(i)){
            is(s_idle){
                when(mem_type_MEM(4,3).orR){
                    cs_backup(i) := Mux(cache_hit(i), s_idle, s_miss)
                    cache_miss(i) := !cache_hit(i)
                }
            }
            is(s_miss){
                cache_miss(i) := true.B
                cs_backup(i) := Mux(io.d_rready && io.d_rlast, s_refill, s_miss)
            }
            is(s_refill){
                cs_backup(i) := s_wait
                cache_miss(i) := true.B
            }
            is(s_wait){
                cs_backup(i) := Mux(w_finish, s_idle, s_wait)
                cache_miss(i) := !w_finish
            }
        }
    }

    // wfsm logic
    val w_count = RegInit(0.U(8.W))
    val cnt_rst = WireDefault(false.B)
    val w_num = (OFFSET_NUM / 4 - 1).U

    when(cnt_rst){
        w_count := w_num
    }.elsewhen(io.d_wvalid && io.d_wready){
        w_count := w_count - 1.U
    }

    val w_idle :: w_write :: w_over :: Nil = Enum(3)
    val wcs = RegInit(w_idle)

    switch(wcs){
        is(w_idle){
            when(wfsm_en){
                wcs := Mux(is_dirty, w_write, w_over)
                cnt_rst := true.B
            }
        }
        is(w_write){
            wcs := Mux(io.d_bvalid, w_over, w_write)
            d_wvalid := !w_count.andR
            d_wlast := w_count === 0.U
            d_bready := true.B
        }
        is(w_over){
            wcs := Mux(wfsm_reset && !stall, w_idle, w_over)
            w_finish := !stall
        }
    }

    // output
    io.cache_miss_iq := VecInit.tabulate(5)(i => mem_type_EX_MEM_backup(i)(4, 3).orR && !hit_MEM(i).orR)

    io.has_store := io.mem_type_RF(4) || mem_type_RF_EX(4)
    io.cache_miss_MEM := cache_miss
    io.rdata_MEM := rdata
    io.d_araddr := addr_MEM(31, OFFSET_WIDTH) ## 0.U(OFFSET_WIDTH.W)
    io.d_rvalid := d_rvalid
    io.d_rsize := 2.U
    io.d_rburst := 1.U
    io.d_rlen := (OFFSET_NUM/4-1).U

    io.d_awaddr := wbuf(31, 0)
    io.d_wdata := wbuf(63, 32)
    io.d_wvalid := d_wvalid
    io.d_wlast := d_wlast
    io.d_wstrb := Fill(4, 1.U(1.W))
    io.d_wsize := 2.U
    io.d_wburst := 1.U
    io.d_wlen := w_num
    io.d_bready := d_bready

    io.commit_dcache_miss := d_miss
    io.commit_dcache_visit := d_visit
}

import chisel3._
import chisel3.util._
import CSR._
import TLB_Struct._
import CPU_Config._
import Exception._

class CSR_Regfile_IO extends Bundle{
    val raddr           = Input(UInt(14.W))//imm
    val rdata           = Output(UInt(32.W))//
    val waddr           = Input(UInt(14.W))//rob.csr_addr
    val we              = Input(Bool())//rob.csr_we
    val wdata           = Input(UInt(32.W))  //rob.csr_wdata

    // exception and ertn
    val exception           = Input(UInt(8.W))
    val badv_exp            = Input(UInt(32.W))
    val is_eret             = Input(Bool())  //是否返回
    val pc_exp              = Input(UInt(32.W))
    val eentry_global       = Output(UInt(32.W))//所有例外入口
    val tlbreentry_global   = Output(UInt(32.W))//tlb的例外入口

    // interrupt
    val interrupt       = Input(UInt(8.W))
    val ip_int          = Input(Bool())
    val interrupt_vec   = Output(UInt(12.W))

    // mmu，接给mmu
    val asid_global     = Output(UInt(10.W))
    val plv_global      = Output(UInt(2.W))
    val tlbehi_global   = Output(UInt(19.W))
    val tlbidx_global   = Output(UInt(log2Ceil(TLB_ENTRY_NUM).W))
    
    val crmd_trans      = Output(UInt(6.W))
    val dmw0_global     = Output(UInt(32.W))
    val dmw1_global     = Output(UInt(32.W))

    // tlbwr
    val tlbentry_global = Output(new tlb_t) 

    // tlbrd
    val tlbentry_in     = Input(new tlb_t)
    val tlbrd_en        = Input(Bool())

    // tlbsrch
    val tlbsrch_en      = Input(Bool())

    // llbit
    val llbit_global    = Output(Bool())
    val llbit_set       = Input(Bool())
    val llbit_clear     = Input(Bool())  

    // debug
    val estat_13        = Output(UInt(13.W))  //这是中断处理，包含
    //核间中断[12]  定时器中断[11]  硬中断[9:2] 软中断[1:0]
}
class CSR_Regfile(PALEN: 32, TIMER_INIT_WIDTH: 30) extends Module{
    val TLB_INDEX_WIDTH = log2Ceil(TLB_ENTRY_NUM)
    val io          = IO(new CSR_Regfile_IO)
    //内部线
    val wdata       = io.wdata
    val waddr       = io.waddr
    val raddr       = io.raddr
    val we          = io.we
    val eret        = io.is_eret
    val interrupt   = io.interrupt
    val trap        = io.exception

    //CRMD
    val crmd        = RegInit(8.U(32.W))
    val prmd        = RegInit(0.U(32.W))
    when(trap(7)){
        crmd := crmd(31,3) ## 0.U(3.W)
    }.elsewhen(eret){
        crmd := crmd(31,3) ## prmd(2,0)
    }.elsewhen(we && waddr === CSR_CRMD){
        crmd := 0.U(23.W) ## wdata(8, 0) //规定前23位0，软件不可写
    }
    io.plv_global  := crmd(1,0)
    io.crmd_trans  := crmd(8, 3)
    
    //PRMD更新逻辑，触发例外前逻辑
    when(trap(7)){ //触发中断，PPLV，PIE写为PLV，IE
        prmd := prmd(31, 3) ## crmd(2, 0)
    }.elsewhen(we && waddr === CSR_PRMD){ //写入PRMD
        prmd := 0.U(29.W) ## wdata(2, 0)
    }

    // EUEN：扩展部件使能
    val euen = RegInit(0.U(32.W))
    when(we && waddr === CSR_EUEN){
        euen := 0.U(31.W) ## wdata(0)
    }

    // ECFG：例外控制
    val ecfg = RegInit(0.U(32.W))
    when(we && waddr === CSR_ECFG){
        ecfg := 0.U(19.W) ## wdata(12, 11) ## 0.U(1.W) ## wdata(9, 0)
    }

    // ESTAT：例外状态
    val estat = RegInit(0.U(32.W))
    val timer_reg   = RegInit(false.B)
    val timer       = timer_reg
    when(trap(7)){//触发例外，将例外状态写入
        estat := 0.U(9.W) ## trap(6,0) ##estat(15,0)  
    }.elsewhen(we && waddr === CSR_ESTAT){
        estat := 0.U(1.W) ## estat(30, 16) ## 0.U(3.W) ## io.ip_int ## timer ## 0.U(1.W) ## io.interrupt ## wdata(1, 0)
    }.otherwise{
        estat := 0.U(1.W) ## estat(30, 16) ## 0.U(3.W) ## io.ip_int ## timer ## 0.U(1.W) ## io.interrupt ## estat(1, 0)
    }
    io.estat_13 := estat(13, 0)

    
    // ERA：例外返回地址,触发例外时，指令的PC记录在这
    val era = RegInit(0.U(32.W))
    when(trap(7)){
        era := io.pc_exp
    }.elsewhen(we && waddr === CSR_ERA){
        era := wdata
    }

    // BADV：出错虚地址
    val save_v  = trap(5, 0) === 0x8.U || trap(6, 0) === ALE
    val badv = RegInit(0.U(32.W))
    when(trap(7) && save_v){
        badv := io.badv_exp
    }.elsewhen(we && waddr === CSR_BADV){
        badv := wdata
    }

    // EENTRY：例外入口地址
    val eentry = RegInit(0.U(32.W))
    when(we && waddr === CSR_EENTRY){
        eentry := wdata(31, 6) ## 0.U(6.W)
    }

    // CPUID：处理器编号
    val coreid = RegInit(0.U(32.W))
    when(we && waddr === CSR_CPUID){
        coreid := 0.U(23.W) ## coreid(8, 0)
    }

    // SAVE0：数据保存
    val data0 = RegInit(0.U(32.W))
    when(we && waddr === CSR_SAVE0){
        data0 := wdata
    }

    // SAVE1：数据保存
    val data1 = RegInit(0.U(32.W))
    when(we && waddr === CSR_SAVE1){
        data1 := wdata
    }

    // SAVE2：数据保存
    val data2 = RegInit(0.U(32.W))
    when(we && waddr === CSR_SAVE2){
        data2 := wdata
    }

    // SAVE3：数据保存
    val data3 = RegInit(0.U(32.W))
    when(we && waddr === CSR_SAVE3){
        data3 := wdata
    }

    // LLBCTL：LLBit控制
    val llbctl = RegInit(0.U(32.W))
    when(io.llbit_set){
        llbctl := 0.U(29.W) ## llbctl(2) ## 1.U(2.W)
    }.elsewhen(io.llbit_clear){
        llbctl := 0.U(29.W) ## llbctl(2) ## 0.U(2.W)
    }.elsewhen(eret){
        llbctl := 0.U(31.W) ## Mux(llbctl(2), llbctl(0), 0.U(1.W))
    }.elsewhen(we && waddr === CSR_LLBCTL){
        llbctl := 0.U(29.W) ## wdata(2) ## 0.U(1.W) ## Mux(wdata(1), 0.U(1.W), llbctl(0)) 
    }

    val tlbentry_in = io.tlbentry_in
    io.llbit_global := llbctl(0)

    // TLBIDX：TLB索引
    val tlbidx = RegInit(0.U(32.W))
    tlbidx := 0.U(32.W)//feng

    io.tlbidx_global := tlbidx(TLB_INDEX_WIDTH-1, 0)

    // TLBEHI：TLB表项高位
    val tlbehi = RegInit(0.U(32.W))
    tlbehi := 0.U(32.W)//feng

    io.tlbehi_global := tlbehi(31, 13)

    // TLBELO0：TLB表项低位
    val tlbelo0 = RegInit(0.U(32.W))
    tlbelo0 := 0.U(32.W)//feng
    
    // TLBELO1：TLB表项低位
    val tlbelo1 = RegInit(0.U(32.W))
    tlbelo1 := 0.U(32.W)//feng


    // ASID：地址空间标识符
    val asid = RegInit(0.U(32.W))
    asid := 0.U(32.W)//feng

    io.asid_global := asid(9, 0)

    // PGDL：低半地址空间全局目录基址
    val pgdl = RegInit(0.U(32.W))
    when(we && waddr === CSR_PGDL){
        pgdl := wdata(31, 12) ## 0.U(12.W)
    }

    // PGDH：高半地址空间全局目录基址
    val pgdh = RegInit(0.U(32.W))
    when(we && waddr === CSR_PGDH){
        pgdh := wdata(31, 12) ## 0.U(12.W)
    }
    
    // PGD：全局目录基址
    val pgd = RegInit(0.U(32.W))
    when(we && waddr === CSR_PGD){
        pgd := wdata(31, 12) ## 0.U(12.W)
    }

    // TLBRENTRY：TLB表项重填例外入口地址
    val tlbreentry = RegInit(0.U(32.W))
    tlbreentry := 0.U(32.W)//feng

    // DMW0：直接映射窗口
    val dmw0 = RegInit(0.U(32.W))
    when(we && waddr === CSR_DMW0){
        dmw0 := wdata(31, 29) ## 0.U(1.W) ## wdata(27, 25) ## 0.U(19.W) ## wdata(5, 3) ## 0.U(2.W) ## wdata(0) 
    }
    io.dmw0_global := dmw0

    // DMW1：直接映射窗口
    val dmw1 = RegInit(0.U(32.W))
    when(we && waddr === CSR_DMW1){
        dmw1 := wdata(31, 29) ## 0.U(1.W) ## wdata(27, 25) ## 0.U(19.W) ## wdata(5, 3) ## 0.U(2.W) ## wdata(0) 
    }
    io.dmw1_global := dmw1

    
    // TID：定时器编号
    val tid = RegInit(0.U(32.W))
    when(we && waddr === CSR_TID){
        tid := wdata
    }

    // TCFG：定时器配置
    val tcfg = RegInit(0.U(32.W))
    when(we && waddr === CSR_TCFG){
        tcfg := 0.U((32 - TIMER_INIT_WIDTH).W) ## wdata(TIMER_INIT_WIDTH - 1, 0)
    }

    // TVAL：定时器数值
    val tval = RegInit(0.U(32.W))
    when(we && waddr === CSR_TCFG){
        tval := 0.U((32 - TIMER_INIT_WIDTH).W) ## wdata(TIMER_INIT_WIDTH - 1, 2) ## 1.U(2.W)
    }.elsewhen(tcfg(0) === 1.U){//定时器倒计时
        when(tval === 0.U){//触发定时器中断信号
            tval := 0.U((32 - TIMER_INIT_WIDTH).W) ## Mux(tcfg(1), tcfg(TIMER_INIT_WIDTH - 1, 2) ## 1.U(2.W), 0.U(TIMER_INIT_WIDTH.W))
        }.otherwise{
            tval := tval - 1.U
        }
    }
      // TICLR：定时器中断清除
    val clr = RegInit(0.U(32.W))  //规定读返回0
    val tval_edge = ShiftRegister(tval, 1)
    when(we && waddr === CSR_TICLR && wdata(0) === 1.U){
        timer_reg := false.B
        clr := 0.U(32.W)
    }.elsewhen(tcfg(0) === 1.U && tval === 0.U && tval_edge === 1.U){
        timer_reg := true.B
        clr := 0.U(32.W)
    }
    io.interrupt_vec := 0.U(12.W)
    io.interrupt_vec := Mux(!crmd(2), 0.U(12.W), (estat(12, 11) & ecfg(12, 11)) ## (estat(9, 0) & ecfg(9, 0)))  

    
    io.tlbentry_global :=  0.U.asTypeOf(new tlb_t)
    
    io.rdata := 0.U(32.W)
    switch(raddr){
        is(CSR_CRMD)        { io.rdata := crmd }
        is(CSR_PRMD)        { io.rdata := prmd }
        is(CSR_EUEN)        { io.rdata := euen }
        is(CSR_ECFG)        { io.rdata := ecfg }   
        is(CSR_ESTAT)       { io.rdata := estat }
        is(CSR_ERA)         { io.rdata := era }
        is(CSR_BADV)        { io.rdata := badv }
        is(CSR_EENTRY)      { io.rdata := eentry }
        is(CSR_CPUID)       { io.rdata := coreid }
        is(CSR_SAVE0)       { io.rdata := data0 }
        is(CSR_SAVE1)       { io.rdata := data1 }
        is(CSR_SAVE2)       { io.rdata := data2 }
        is(CSR_SAVE3)       { io.rdata := data3 }
        is(CSR_LLBCTL)      { io.rdata := llbctl }
        is(CSR_TLBIDX)      { io.rdata := tlbidx }
        is(CSR_TLBEHI)      { io.rdata := tlbehi }
        is(CSR_TLBELO0)     { io.rdata := tlbelo0 }
        is(CSR_TLBELO1)     { io.rdata := tlbelo1 }
        is(CSR_ASID)        { io.rdata := asid }
        is(CSR_PGDL)        { io.rdata := pgdl }
        is(CSR_PGDH)        { io.rdata := pgdh }
        is(CSR_PGD)         { io.rdata := pgd }
        is(CSR_TLBRENTRY)   { io.rdata := tlbreentry }
        is(CSR_DMW0)        { io.rdata := dmw0 }
        is(CSR_DMW1)        { io.rdata := dmw1 }
        is(CSR_TID)         { io.rdata := tid }
        is(CSR_TCFG)        { io.rdata := tcfg }
        is(CSR_TVAL)        { io.rdata := tval }
        is(CSR_TICLR)       { io.rdata := clr }
    }
    io.eentry_global     := eentry
    io.tlbreentry_global := tlbreentry

}
import chisel3._
import chisel3.util._

/* Loongarch 32R */
object Instructions {
    // rdcnt
    def RDCNTIDW    = BitPat("b0000000000000000011000?????00000")
    def RDCNTVLW    = BitPat("b000000000000000001100000000?????")
    def RDCNTVHW    = BitPat("b000000000000000001100100000?????")
    
    // logic reg-reg
    def ADDW        = BitPat("b00000000000100000???????????????")
    def SUBW        = BitPat("b00000000000100010???????????????")
    def SLT         = BitPat("b00000000000100100???????????????")
    def SLTU        = BitPat("b00000000000100101???????????????")
    def NOR         = BitPat("b00000000000101000???????????????")
    def AND         = BitPat("b00000000000101001???????????????")
    def OR          = BitPat("b00000000000101010???????????????")
    def XOR         = BitPat("b00000000000101011???????????????")
    def SLLW        = BitPat("b00000000000101110???????????????")
    def SRLW        = BitPat("b00000000000101111???????????????")
    def SRAW        = BitPat("b00000000000110000???????????????")
    def MULW        = BitPat("b00000000000111000???????????????")
    def MULHW       = BitPat("b00000000000111001???????????????")
    def MULHWU      = BitPat("b00000000000111010???????????????")
    def DIVW        = BitPat("b00000000001000000???????????????")
    def MODW        = BitPat("b00000000001000001???????????????")
    def DIVWU       = BitPat("b00000000001000010???????????????")
    def MODWU       = BitPat("b00000000001000011???????????????")

    // else
    def BREAK       = BitPat("b00000000001010100???????????????")
    def SYSCALL     = BitPat("b00000000001010110???????????????")

    // logic reg-imm
    def SLLIW       = BitPat("b00000000010000001???????????????")
    def SRLIW       = BitPat("b00000000010001001???????????????")
    def SRAIW       = BitPat("b00000000010010001???????????????")
    def SLTI        = BitPat("b0000001000??????????????????????")
    def SLTUI       = BitPat("b0000001001??????????????????????")
    def ADDIW       = BitPat("b0000001010??????????????????????")
    def ANDI        = BitPat("b0000001101??????????????????????")
    def ORI         = BitPat("b0000001110??????????????????????")
    def XORI        = BitPat("b0000001111??????????????????????")

    // CSR
    def CSRRD       = BitPat("b00000100??????????????00000?????")
    def CSRWR       = BitPat("b00000100??????????????00001?????")
    def CSRXCHG     = BitPat("b00000100????????????????????????")

    // cacop
    def CACOP       = BitPat("b0000011000??????????????????????")

    // tlb
    def TLBSRCH     = BitPat("b00000110010010000010100000000000")
    def TLBRD       = BitPat("b00000110010010000010110000000000")
    def TLBWR       = BitPat("b00000110010010000011000000000000")
    def TLBFILL     = BitPat("b00000110010010000011010000000000")

    // priv
    def ERTN        = BitPat("b00000110010010000011100000000000")
    def IDLE        = BitPat("b00000110010010001???????????????")
    def INVTLB      = BitPat("b00000110010010011???????????????")

    // imm and pc
    def LU12IW      = BitPat("b0001010?????????????????????????")
    def PCADDU12I   = BitPat("b0001110?????????????????????????")

    // atmomic
    def LLW         = BitPat("b00100000????????????????????????")
    def SCW         = BitPat("b00100001????????????????????????")

    // load-store
    def LDB         = BitPat("b0010100000??????????????????????")
    def LDH         = BitPat("b0010100001??????????????????????")
    def LDW         = BitPat("b0010100010??????????????????????")
    def STB         = BitPat("b0010100100??????????????????????")
    def STH         = BitPat("b0010100101??????????????????????")
    def STW         = BitPat("b0010100110??????????????????????")
    def LDBU        = BitPat("b0010101000??????????????????????")
    def LDHU        = BitPat("b0010101001??????????????????????")
    
    // branch
    def JIRL        = BitPat("b010011??????????????????????????")
    def B           = BitPat("b010100??????????????????????????")
    def BL          = BitPat("b010101??????????????????????????")
    def BEQ         = BitPat("b010110??????????????????????????")
    def BNE         = BitPat("b010111??????????????????????????")    
    def BLT         = BitPat("b011000??????????????????????????")
    def BGE         = BitPat("b011001??????????????????????????")
    def BLTU        = BitPat("b011010??????????????????????????")
    def BGEU        = BitPat("b011011??????????????????????????")
}


/* CSR */
object CSR {
    val CSR_CRMD        = 0x0.U(14.W)
    val CSR_PRMD        = 0x1.U(14.W)
    val CSR_EUEN        = 0x2.U(14.W)
    val CSR_ECFG        = 0x4.U(14.W)
    val CSR_ESTAT       = 0x5.U(14.W)
    val CSR_ERA         = 0x6.U(14.W)
    val CSR_BADV        = 0x7.U(14.W)
    val CSR_EENTRY      = 0xc.U(14.W)
    val CSR_TLBIDX      = 0x10.U(14.W)
    val CSR_TLBEHI      = 0x11.U(14.W)
    val CSR_TLBELO0     = 0x12.U(14.W)
    val CSR_TLBELO1     = 0x13.U(14.W)
    val CSR_ASID        = 0x18.U(14.W)
    val CSR_PGDL        = 0x19.U(14.W)
    val CSR_PGDH        = 0x1a.U(14.W)
    val CSR_PGD         = 0x1b.U(14.W)
    val CSR_CPUID       = 0x20.U(14.W)
    val CSR_SAVE0       = 0x30.U(14.W)
    val CSR_SAVE1       = 0x31.U(14.W)
    val CSR_SAVE2       = 0x32.U(14.W)
    val CSR_SAVE3       = 0x33.U(14.W)
    val CSR_TID         = 0x40.U(14.W)
    val CSR_TCFG        = 0x41.U(14.W)
    val CSR_TVAL        = 0x42.U(14.W)
    val CSR_TICLR       = 0x44.U(14.W)
    val CSR_LLBCTL      = 0x60.U(14.W)
    val CSR_TLBRENTRY   = 0x88.U(14.W)
    val CSR_CTAG        = 0x98.U(14.W)
    val CSR_DMW0        = 0x180.U(14.W)
    val CSR_DMW1        = 0x181.U(14.W)
}

/* Exception */
object Exception{
    val INT     = 0x00.U(7.W) // interrupt
    val PIL     = 0x01.U(7.W) // page illegal load
    val PIS     = 0x02.U(7.W) // page illegal store
    val PIF     = 0x03.U(7.W) // page illegal fetch
    val PME     = 0x04.U(7.W) // page maintain exception
    val PPI     = 0x07.U(7.W) // page privilege illegal
    val ADEF    = 0x08.U(7.W) // address exception fetch
    val ADEM    = 0x48.U(7.W) // address exception memory
    val ALE     = 0x09.U(7.W) // address align exception
    val SYS     = 0x0b.U(7.W) // system call
    val BRK     = 0x0c.U(7.W) // breakpoint
    val INE     = 0x0d.U(7.W) // instruction not exist
    val IPE     = 0x0e.U(7.W) // instruction privilege exception
    val FPD     = 0x0f.U(7.W) // floating point disable
    val FPE     = 0x12.U(7.W) // floating point exception
    val TLBR    = 0x3F.U(7.W) // TLB refill
}




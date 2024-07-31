import chisel3._
import chisel3.util._
import CPU_Config._

class Free_List_IO(n: Int) extends Bundle{
    val rd_valid            = Input(Vec(2, Bool()))         // valid instruction
    val rename_en           = Input(Vec(2, Bool()))         // rename enable
    
    val commit_en           = Input(Vec(2, Bool()))         // commit enable
    val commit_pprd_valid   = Input(Vec(2, Bool()))         // commit valid
    val commit_pprd         = Input(Vec(2, UInt(log2Ceil(n).W)))        // commit physical register
    
    val predict_fail        = Input(Bool())                 // predict fail
    val head_arch           = Input(UInt(log2Ceil(n).W))        // head arch register

    val alloc_preg          = Output(Vec(2, UInt(log2Ceil(n).W)))     // allocate physical register
    val empty               = Output(Bool())                        // empty
}

class Free_List(n: Int) extends Module{
    val io = IO(new Free_List_IO(n))

    def rotate_left_1(x: UInt): UInt = {
        val n = x.getWidth
        Cat(x(n-2, 0), x(n-1))
    }

    val free_list  = RegInit(VecInit.tabulate(n)(i => (i + 1).asUInt(log2Ceil(n).W)))
    val head = RegInit(1.U(n.W))
    val rear = Reg(UInt(n.W))
    val rear_init = VecInit.tabulate(n)(i => (i.U === (n-1).U)).asUInt

    
    io.empty := (head === rear) || (rotate_left_1(head) === rear)

    //Dequeue -- allocate new physical register
    val head_pre    = Wire(Vec(2, UInt(n.W)))
    var head_cur    = head
    io.alloc_preg  := VecInit.fill(2)(0.U(log2Ceil(n).W))
    for(i <- 0 until 2){
        head_pre(i)         := head_cur
        head_cur             = Mux(io.rd_valid(i) && io.rename_en(i) && !io.empty, rotate_left_1(head_cur), head_cur)
        io.alloc_preg(i)    := Mux1H(head_pre(i), free_list)
    }
    head := head_cur

    //Enqueue -- push retired physical register to free list
    val rear_prev = Wire(Vec(2, UInt(n.W)))
    var rear_curr = rear
    for(i <- 0 until 2){
        rear_prev(i) := rear_curr
        rear_curr = Mux(io.commit_en(i) && io.commit_pprd_valid(i), rotate_left_1(rear_curr), rear_curr)
        when(io.commit_en(i) && io.commit_pprd_valid(i)){
            free_list(OHToUInt(rear_prev(i))) := io.commit_pprd(i)
        }
    }
    rear := rear_curr

    when(reset.asBool){
        rear := rear_init
    }

    when(io.predict_fail){
        head := UIntToOH(io.head_arch)
    }
}

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
    val inst_valid = io.rd_valid
    val rename_en = io.rename_en
    val commit_en = io.commit_en
    val commit_valid = io.commit_pprd_valid
    val commit_pprd = io.commit_pprd
    val head_arch = io.head_arch
    val predict_fail = io.predict_fail

    def rotate_left_1(x: UInt): UInt = {
        val n = x.getWidth
        Cat(x(n-2, 0), x(n-1))
    }

    val free_list  = RegInit(VecInit.tabulate(n)(i => (i + 1).asUInt(log2Ceil(n).W)))
    val free_list   = RegInit(VecInit.tabulate(n)(i => (i + 1).asUInt(log2Ceil(n)-1, 0)))
    val head = RegInit(1.U(n.W))
    val rear = Reg(UInt(n.W))

    when(reset.asBool){
        rear := Cat(1.U(1.W), 0.U((n-1).W))
    }

    when(predict_fail){
        head := UIntToOH(head_arch)
    }
    
    val empty = Wire(Bool())
    empty := (head === rear) || (rotate_left_1(head) === rear)
    io.empty := empty

    //Dequeue -- allocate new physical register
    val head_prev = Wire(Vec(2, UInt(n.W)))
    var head_curr = head
    val alloc_preg = Wire(VecInit.fill(2)(0.U(log2Ceil(n).W)))
    for(i <- 0 until 2){
        head_prev(i) := head_curr
        when(inst_valid(i) && rename_en(i) && !empty){
            head_curr = rotate_left_1(head_curr)
        }
        alloc_preg(i) := Mux1H(head_prev(i), free_list)
    }
    head := head_curr
    io.alloc_preg := alloc_preg

    //Enqueue -- push retired physical register to free list
    val rear_prev = Wire(Vec(2, UInt(n.W)))
    var rear_curr = rear
    for(i <- 0 until 2){
        rear_prev(i) := rear_curr
        when(commit_en(i) && commit_valid(i)){
            rear_curr := rotate_left_1(rear_curr)
            free_list(OHToUInt(rear_prev(i))) := commit_pprd(i)
        }
    }
    rear := rear_curr
}

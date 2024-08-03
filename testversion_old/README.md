这里是测试版本，可编译

/home/feng1702/longarc/test_version/src/main/scala/00-CPU/CPU.scala:163:91: value has_cacop_IF is not a member of ICache_IO
[error]     pc.io.pc_stall                  := fq.io.full || icache.io.cache_miss_RM || icache.io.has_cacop_IF

[error]                                                                                           ^
[error] /home/feng1702/longarc/test_version/src/main/scala/00-CPU/CPU.scala:198:91: value has_cacop_IF is not a member of ICache_IO
[error]     pi_reg.io.stall                 := fq.io.full || icache.io.cache_miss_RM || icache.io.has_cacop_IF
[error]     
                                                                                      ^
[error] /home/feng1702/longarc/test_version/src/main/scala/00-CPU/CPU.scala:207:15: value uncache_IF is not a member of ICache_IO
[error]     icache.io.uncache_IF            := mmu.io.i_uncache 
[error]               ^

[error] /home/feng1702/longarc/test_version/src/main/scala/00-CPU/CPU.scala:213:15: value cacop_en is not a member of ICache_IO
[error]     icache.io.cacop_en              := RegNext(re_reg4.io.inst_pack_EX.priv_vec(0)) && RegNext(re_reg4.io.inst_pack_EX.imm(2, 0) === 0.U)
[error]               ^

[error] /home/feng1702/longarc/test_version/src/main/scala/00-CPU/CPU.scala:214:15: value cacop_op is not a member of ICache_IO
[error]     icache.io.cacop_op              := RegNext(re_reg4.io.inst_pack_EX.imm(4, 3))
[error]               ^

以上报错被注释修改
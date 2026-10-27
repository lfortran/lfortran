submodule (submodule_62_m2) submodule_62_t
    use submodule_62_r
    use submodule_62_q
    implicit none
contains
    module procedure t_step
        r = r_bump()
        q = q_bump()
    end procedure

    module procedure t_q_total
        n = q_total
    end procedure
end submodule

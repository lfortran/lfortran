submodule (submodule_63_m:submodule_63_s1) submodule_63_s2
    use submodule_63_q
    implicit none
contains
    module procedure s2_value
        v = q_value() + k_twice()
    end procedure
end submodule

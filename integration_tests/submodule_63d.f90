submodule (submodule_63_m) submodule_63_s1
    use submodule_63_k
    implicit none
contains
    module procedure s1_value
        v = k_twice()
    end procedure
end submodule

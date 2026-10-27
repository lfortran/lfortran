submodule (submodule_58_m:submodule_58_s) submodule_58_s2
    implicit none
contains
    module procedure get_label
        if (s_helper() /= 2) error stop 5
        r = n_tag // "-s2"
    end procedure
end submodule

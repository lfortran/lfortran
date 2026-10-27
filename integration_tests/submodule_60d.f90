! Reads the variables of its parent submodule before any procedure of the
! parent submodule has run.
submodule (submodule_60_m:submodule_60_s) submodule_60_s2
    implicit none
    type(tc) :: objc2(2)
contains
    module procedure s2_check
        code = 0
        if (objc2(1)%nm /= "abcd" .or. objc2(2)%k /= 3) code = code + 1
        if (objb(2)%nm /= "wxyz" .or. objb(2)%k /= 5) code = code + 2
        if (objc(1)%nm /= "abcd") code = code + 4
        if (associated(obj%q)) code = code + 8
    end procedure
end submodule

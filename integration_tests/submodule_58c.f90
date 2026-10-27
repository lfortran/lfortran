! Uses a procedure of its parent and a procedure and a character
! variable of a module that its parent uses, which the program does not use.
submodule (submodule_58_m) submodule_58_s
    implicit none
    integer :: calls = 0
contains
    module function get_value() result(r)
        integer :: r
        calls = calls + 1
        r = priv_helper(counter)
    end function

    module procedure bump
        counter = counter + twice(k)
        label = "bumped"
        calls = calls + 1
    end procedure

    integer function s_helper()
        s_helper = calls
    end function
end submodule

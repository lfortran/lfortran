submodule (mod_separate_compilation_22) submod_separate_compilation_22
contains
    module function f_sc_22(a) result(r)
    integer, intent(in) :: a
    integer :: r
    r = a + 1
    end function
end submodule
submodule (mod_separate_compilation_18) submod_separate_compilation_18
contains
    module function f(a) result(r)
    integer, intent(in) :: a
    integer :: r
    r = a + 1
    end function
end submodule
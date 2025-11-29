module mod_cpp_mod
contains
    subroutine boom(a)
        real, allocatable, intent(inout) :: a(:)
        ! Force runtime path that needs preprocessor location remap
        a = 1.0
    end subroutine boom
end module mod_cpp_mod

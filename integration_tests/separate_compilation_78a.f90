module separate_compilation_78a
    implicit none
    integer :: calls = 0
contains
    integer function triple(x)
        integer, intent(in) :: x
        triple = 3*x
        calls = calls + 1
    end function
end module

module lfortran_intrinsic_builtin
implicit none

interface

    logical function present(x)
    integer, optional, intent(in) :: x
    end function

end interface

end module

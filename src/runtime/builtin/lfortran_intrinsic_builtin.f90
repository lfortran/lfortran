module lfortran_intrinsic_builtin
implicit none

interface

    integer function int(x, kind)
    real, intent(in) :: x(:)
    integer, optional, intent(in) :: kind
    end function

    logical function present(x)
    integer, optional, intent(in) :: x
    end function

end interface

end module

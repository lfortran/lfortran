module lfortran_intrinsic_builtin
implicit none

interface

    integer function int(x, kind)
    real, intent(in) :: x(:)
    integer, optional, intent(in) :: kind
    end function

    integer function len(x, kind)
    character(len=*), intent(in) :: x
    integer, optional, intent(in) :: kind
    end function

    logical function present(x)
    integer, optional, intent(in) :: x
    end function

    logical function is_iostat_eor(i) result(r)
    integer, intent(in) :: i
    end function

    logical function is_iostat_end(i) result(r)
    integer, intent(in) :: i
    end function

end interface

end module

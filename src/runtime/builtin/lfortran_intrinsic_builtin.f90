module lfortran_intrinsic_builtin
implicit none

interface
    subroutine move_alloc(from, to)
    integer, allocatable, intent(inout) :: from
    integer, allocatable, intent(out) :: to
    end subroutine

    integer function shape(x)
    integer, intent(in) :: x(:)
    end function

    integer function reshape(x, shape_vec) result(r)
    integer, intent(in) :: x(:)
    integer, intent(in) :: shape_vec(:)
    end function

    integer function lbound(x, dim)
    integer, intent(in) :: x(:)
    integer, intent(in) :: dim
    end function

    integer function ubound(x, dim)
    integer, intent(in) :: x(:)
    integer, intent(in) :: dim
    end function

    integer function max(a, b)
    integer, intent(in) :: a, b
    end function

    integer function min(a, b)
    integer, intent(in) :: a, b
    end function

    logical function allocated(x)
    integer, intent(in) :: x(:)
    end function

    integer function minval(x)
    integer, intent(in) :: x(:)
    end function

    integer function maxval(x)
    integer, intent(in) :: x(:)
    end function

    integer function sum(x)
    integer, intent(in) :: x(:)
    end function

    real function tiny(x)
    integer, intent(in) :: x(:)
    end function

    real function real(x, kind)
    integer, intent(in) :: x(:)
    integer, intent(in) :: kind
    end function

    integer function int(x, kind)
    real, intent(in) :: x(:)
    integer, intent(in) :: kind
    end function

    character function char(x)
    integer, intent(in) :: x
    end function

    integer function len(x, kind)
    character(len=*), intent(in) :: x
    integer, optional, intent(in) :: kind
    end function

    logical function present(x)
    integer, optional, intent(in) :: x
    end function

    integer function bit_size(x)
    integer, intent(in) :: x
    end function

    integer function iachar(c, kind)
    character(len=1), intent(in) :: c
    integer, optional :: kind
    end function

    character(len=1) function achar(i, kind)
    integer, intent(in) :: i
    integer, optional :: kind
    end function

    logical function any(mask, dim) result(r)
    logical, intent(in) :: mask(:)
    integer(4), optional :: dim
    end function

    logical function is_iostat_eor(i) result(r)
    integer, intent(in) :: i
    end function

    logical function is_iostat_end(i) result(r)
    integer, intent(in) :: i
    end function

end interface

end module

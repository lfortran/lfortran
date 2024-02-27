module lfortran_intrinsic_bit
use, intrinsic :: iso_fortran_env, only: int8, int16, int32, int64
implicit none

interface mvbits
    module procedure mvbits32, mvbits64
end interface

interface ibits
    module procedure ibits32, ibits64
end interface

contains

! mvbits ------------------------------------------------------------------------

elemental subroutine mvbits32(from, frompos, len, to, topos)
integer(int32), intent(in) :: from, frompos, len, topos
integer(int32), intent(out) :: to
interface
    pure integer(int32) function c_mvbits32(from, frompos, len, to, topos) bind(c, name="_lfortran_mvbits32")
    import :: int32
    integer(int32), intent(in), value :: from, frompos, len, to, topos
    end function
end interface
to = c_mvbits32(from, frompos, len, to, topos)
end subroutine

elemental subroutine mvbits64(from, frompos, len, to, topos)
integer(int64), intent(in) :: from
integer(int32), intent(in) :: frompos, len, topos
integer(int64), intent(out) :: to
interface
    pure integer(int64) function c_mvbits64(from, frompos, len, to, topos) bind(c, name="_lfortran_mvbits64")
    import :: int64, int32
    integer(int64), intent(in), value :: from, to
    integer(int32), intent(in), value :: frompos, len, topos
    end function
end interface
to = c_mvbits64(from, frompos, len, to, topos)
end subroutine

! ibits ------------------------------------------------------------------------

elemental integer(int32) function ibits32(i, pos, len) result(r)
integer(int32), intent(in) :: i, pos, len
interface
    pure integer(int32) function c_ibits32(i, pos, len) bind(c, name="_lfortran_ibits32")
    import :: int32
    integer(int32), intent(in), value :: i, pos, len
    end function
end interface
r = c_ibits32(i, pos, len)
end function

elemental integer(int64) function ibits64(i, pos, len) result(r)
integer(int64), intent(in) :: i
integer(int32), intent(in) :: pos, len
interface
    pure integer(int64) function c_ibits64(i, pos, len) bind(c, name="_lfortran_ibits64")
    import :: int32, int64
    integer(int64), intent(in), value :: i
    integer(int32), intent(in), value :: pos, len
    end function
end interface
r = c_ibits64(i, pos, len)
end function

function count(mask, dim, kind) result(r)
logical :: mask(:)
integer, optional :: dim, kind
integer :: r(size(mask))
end function

end module

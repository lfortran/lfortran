module derived_types_186_mod
implicit none

type :: deep
    integer :: dd = 7
end type deep

type :: inner
    integer :: ii = 9
    type(deep) :: d
end type inner

type :: outer
    integer :: id = 0
    type(inner) :: nest
end type outer

contains

    subroutine read_assumed_shape(a)
    type(outer), intent(in) :: a(:)
    ! a whole-array read of a nested member of an assumed-shape dummy
    if (any(a%nest%ii /= 9)) error stop "assumed shape: wrong values read"
    if (sum(a%nest%ii) /= 9 * size(a)) error stop "assumed shape: wrong sum"
    if (any(a%nest%d%dd /= 7)) error stop "assumed shape: three levels read"
    if (a(1)%nest%ii /= 9) error stop "assumed shape: index first"
    end subroutine read_assumed_shape

    subroutine write_assumed_shape(a, val)
    type(outer), intent(inout) :: a(:)
    integer, intent(in) :: val
    a%nest%ii = val
    a%nest%d%dd = val + 1
    end subroutine write_assumed_shape

    subroutine read_explicit_shape(a, n)
    integer, intent(in) :: n
    type(outer), intent(in) :: a(n)
    if (any(a%nest%ii /= 4)) error stop "explicit shape: wrong values read"
    if (any(a%nest%d%dd /= 5)) error stop "explicit shape: three levels read"
    end subroutine read_explicit_shape

    integer function total(a)
    type(outer), intent(in) :: a(:)
    total = sum(a%nest%d%dd)
    end function total

end module derived_types_186_mod

program derived_types_186
use derived_types_186_mod
implicit none

type(outer) :: v(3)

call read_assumed_shape(v)

call write_assumed_shape(v, 4)
if (v(1)%nest%ii /= 4) error stop "copy-out: element 1 not written"
if (v(2)%nest%ii /= 4) error stop "copy-out: element 2 not written"
if (v(3)%nest%ii /= 4) error stop "copy-out: element 3 not written"
if (any(v%nest%d%dd /= 5)) error stop "copy-out: three levels not written"
if (any(v%id /= 0)) error stop "copy-out: neighbouring component clobbered"

call read_explicit_shape(v, 3)
if (total(v) /= 15) error stop "function result: wrong sum"

! a section of the array passed as the dummy
v%nest%ii = 9
v%nest%d%dd = 7
call read_assumed_shape(v(1:2))
call read_assumed_shape(v(1:3:2))

print *, v%nest%ii
print *, v%nest%d%dd
end program derived_types_186

! A polymorphic allocatable array whose declared type has allocatable
! components, reallocated on assignment to a different size: the old
! elements are finalized and the array gets new storage.
module class_167_m
implicit none
type :: base_t
    integer, allocatable :: v(:)
end type
type :: holder_t
    class(base_t), allocatable :: arr(:)
end type
contains
function mk_base(n) result(r)
    integer, intent(in) :: n
    type(base_t), allocatable :: r(:)
    integer :: i, j
    allocate(r(n))
    do i = 1, n
        r(i)%v = [(10*i + j, j = 1, i)]
    end do
end function
subroutine check(t, n)
    class(base_t), intent(in) :: t(:)
    integer, intent(in) :: n
    integer :: i
    if (size(t) /= n) error stop 1
    do i = 1, n
        if (size(t(i)%v) /= i) error stop 2
        if (t(i)%v(i) /= 11*i) error stop 3
    end do
end subroutine
end module

program class_167
use class_167_m
implicit none
class(base_t), allocatable :: t(:)
type(holder_t) :: h

t = mk_base(3)
call check(t, 3)
t = mk_base(2)
call check(t, 2)
t = mk_base(4)
call check(t, 4)

h%arr = mk_base(2)
call check(h%arr, 2)
h%arr = mk_base(5)
call check(h%arr, 5)
print *, "ok"
end program

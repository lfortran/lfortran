module char_28_mod
implicit none
interface operator (.exclude.)
   module procedure char_exclude
end interface
contains

function char_exclude(values, drop) result(kept)
character(len=*), intent(in) :: values(:), drop(:)
character(len=len(values(1))), allocatable :: kept(:)
integer :: i, n
n = 0
do i = 1, size(values)
   if (.not. any(values(i) == drop)) n = n + 1
end do
allocate(kept(n))
n = 0
do i = 1, size(values)
   if (.not. any(values(i) == drop)) then
      n = n + 1
      kept(n) = values(i)
   end if
end do
end function char_exclude

end module char_28_mod

program char_28
use char_28_mod, only: operator(.exclude.)
implicit none

character(len=3), allocatable :: res(:)

res = ["dog","cat","boy"] .exclude. ["cat "]

if (size(res) /= 2) error stop "incorrect size"
if (res(1) /= "dog") error stop "incorrect first value"
if (res(2) /= "boy") error stop "incorrect second value"

write (*,"(100(a,1x))") res
print *, "test passed"

end program char_28
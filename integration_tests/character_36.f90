! Regression test for https://github.com/lfortran/lfortran/issues/15107
! PACK of an assumed-length character array passed to a subroutine with an
! allocatable intent(out) assumed-length dummy, inside a function returning
! an allocatable character array.
module character_36_mod
implicit none
contains
function diff_vec_char(xx,yy) result(zz)
character(len=*), intent(in) :: xx(:),yy(:)
character(len=len(xx)), allocatable :: zz(:)
logical :: xuse(size(xx))
integer :: i
do i=1,size(xx)
   xuse(i) = all(yy /= xx(i))
end do
call set_alloc(pack(xx,xuse),zz)
end function diff_vec_char

pure subroutine set_alloc(xx,yy)
character(len=*), intent(in) :: xx(:)
character(len=*), allocatable, intent(out) :: yy(:)
allocate(yy(size(xx)))
yy = xx
end subroutine set_alloc
end module character_36_mod

program character_36
use character_36_mod, only: diff_vec_char
implicit none
write(*,"(100(a,1x))") diff_vec_char(["dog","cat","boy"], ["cat "])
if (size(diff_vec_char(["dog","cat","boy"], ["cat "])) /= 2) error stop
if (len(diff_vec_char(["dog","cat","boy"], ["cat "])) /= 3) error stop
if (any(diff_vec_char(["dog","cat","boy"], ["cat "]) /= ["dog","boy"])) error stop
if (size(diff_vec_char(["alpha","beta ","gamma"], ["alpha","gamma"])) /= 1) error stop
if (len(diff_vec_char(["alpha","beta ","gamma"], ["alpha","gamma"])) /= 5) error stop
if (any(diff_vec_char(["alpha","beta ","gamma"], ["alpha","gamma"]) /= ["beta "])) error stop
if (size(diff_vec_char(["x","y"], ["x","y"])) /= 0) error stop
end program character_36

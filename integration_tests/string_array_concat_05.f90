! Regression test for https://github.com/lfortran/lfortran/issues/15249
! A zero-size allocated character array passed to a function whose result
! length is max(len(xx),len(yy)) must not be indexed when evaluating LEN.
module string_array_concat_05_mod
implicit none
contains
pure function concat_char_vec(xx,yy) result(zz)
character(len=*), intent(in) :: xx(:),yy(:)
character(len=max(len(xx),len(yy))) :: zz(size(xx)+size(yy))
integer :: nx
nx=size(xx)
zz(:nx)=xx
zz(nx+1:)=yy
end function concat_char_vec

pure subroutine set_alloc_character_vec(xx,yy,nsize)
character(len=*), intent(in) :: xx(:)
character(len=*), intent(out), allocatable :: yy(:)
integer, intent(out), optional :: nsize
allocate(yy(size(xx)))
yy=xx
if(present(nsize)) nsize=size(yy)
end subroutine set_alloc_character_vec
end module string_array_concat_05_mod

program string_array_concat_05
use string_array_concat_05_mod
implicit none
character(len=10), allocatable :: xchar(:),char_vec(:)
integer :: n
allocate(xchar(0))
call set_alloc_character_vec(concat_char_vec(xchar,['four','five','zero']),char_vec)
print *,size(char_vec),len(char_vec)
if (size(char_vec) /= 3) error stop
if (len(char_vec) /= 10) error stop
if (char_vec(1) /= 'four') error stop
if (char_vec(2) /= 'five') error stop
if (char_vec(3) /= 'zero') error stop
if (trim(char_vec(1))//'|'//trim(char_vec(2))//'|'//trim(char_vec(3)) /= 'four|five|zero') error stop

deallocate(char_vec)
call set_alloc_character_vec(concat_char_vec(['ab','cd'],xchar),char_vec,n)
if (n /= 2) error stop
if (size(char_vec) /= 2) error stop
if (len(char_vec) /= 10) error stop
if (char_vec(1) /= 'ab' .or. char_vec(2) /= 'cd') error stop
end program string_array_concat_05

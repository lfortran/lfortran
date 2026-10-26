! Repeated format group with an implied-do output list of trimmed
! assumed-shape array elements must stay on one record (issue #15955)
module format_119_mod
implicit none
contains
subroutine show(u, words)
integer, intent(in) :: u
character(len=*), intent(in) :: words(:)
integer :: i
write(u,'(100(a,1x))') (trim(words(i)),i=1,size(words))
end subroutine

subroutine show_stdout(words)
character(len=*), intent(in) :: words(:)
integer :: i
write(*,'(100(a,1x))') (trim(words(i)),i=1,size(words))
end subroutine

subroutine show_internal(words, line)
character(len=*), intent(in) :: words(:)
character(len=*), intent(out) :: line
integer :: i
write(line,'(100(a,1x))') (trim(words(i)),i=1,size(words))
end subroutine

subroutine check_unit(words, expected)
character(len=*), intent(in) :: words(:)
character(len=*), intent(in) :: expected
character(len=40) :: line
integer :: u, ios
open(newunit=u, status='scratch', action='readwrite')
call show(u, words)
rewind(u)
read(u,'(a)') line
if (trim(line) /= expected) error stop 3
read(u,'(a)',iostat=ios) line
if (ios >= 0) error stop 4
close(u)
end subroutine
end module

program format_119
use format_119_mod
implicit none
character(len=20) :: line
call show_stdout(['min','max'])
call check_unit(['min','max'], 'min max')
call show_internal(['min','max'], line)
print '(a)', trim(line)
if (line /= 'min max') error stop 1
call show_stdout(['alpha','b    ','gamma'])
call check_unit(['alpha','b    ','gamma'], 'alpha b gamma')
call show_internal(['alpha','b    ','gamma'], line)
print '(a)', trim(line)
if (line /= 'alpha b gamma') error stop 2
end program

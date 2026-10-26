! Repeated format group with an implied-do output list of trimmed
! assumed-shape array elements must stay on one record (issue #15955)
module format_119_mod
implicit none
contains
subroutine show(words)
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
end module

program format_119
use format_119_mod
implicit none
character(len=20) :: line
call show(['min','max'])
call show_internal(['min','max'], line)
print '(a)', trim(line)
if (line /= 'min max') error stop 1
call show(['alpha','b    ','gamma'])
call show_internal(['alpha','b    ','gamma'], line)
print '(a)', trim(line)
if (line /= 'alpha b gamma') error stop 2
end program

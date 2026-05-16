program file_71
implicit none
integer :: i

open(23, status="scratch")
write(23,'(a)') "Line 1"
write(23,'(a)') "Line 2"
write(23,'(a)') "Line 3"

rewind(23)

do i = 1, 10
    read(23,'(1x)', end=12)
end do

12 continue
if (i /= 4) error stop

end program file_71
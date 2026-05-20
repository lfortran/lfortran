program file_open_08
! Test that open(file=var) does not corrupt the file= variable
implicit none
character(len=20) :: fname
integer :: i

fname = "file_open_08.tmp"
open(unit=10, file=fname)
close(10, status="delete")

! After open, all trailing characters must still be spaces (char code 32),
! not null bytes (char code 0).
do i = 17, 20
    if (iachar(fname(i:i)) /= 32) error stop
end do
print *, "PASS"
end program

program read_86
implicit none
integer, parameter :: dp = kind(1.0d0)
real(kind=dp) :: xread(4)
integer :: i, ierr, nread_, max_read_
character(len=*), parameter :: text = "1.5 2.5 3.5"

max_read_ = 4
nread_ = 0
do i = max_read_, 1, -1
    read(text, *, iostat=ierr) xread(:i)
    if (ierr == 0) then
        nread_ = i
        exit
    end if
end do
print *, "nread_ =", nread_
if (nread_ /= 3) error stop
if (abs(xread(1) - 1.5_dp) > 1.0e-12_dp) error stop
if (abs(xread(2) - 2.5_dp) > 1.0e-12_dp) error stop
if (abs(xread(3) - 3.5_dp) > 1.0e-12_dp) error stop
end program

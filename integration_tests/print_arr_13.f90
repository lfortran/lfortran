! A SIMD array is formatted like any other array
program print_arr_13
implicit none
!LF$ attributes simd :: v
real :: v(8)
character(len=80) :: buf
integer :: i
do i = 1, 8
    v(i) = i
end do
print *, v
write(buf, '(8F4.0)') v
if (trim(buf) /= '  1.  2.  3.  4.  5.  6.  7.  8.') error stop
end program

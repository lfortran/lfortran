! A format given as an allocatable or sectioned character array
program print_arr_15
implicit none
character(len=4), allocatable :: fa(:)
character(len=4) :: fm(3)
character(len=40) :: buf
integer :: x(3) = [1, 2, 3]
allocate(fa(2))
fa(1) = '(3I3'
fa(2) = ')'
print fa, x
fm(1) = '(3I3'
fm(2) = 'XXXX'
fm(3) = ')'
print fm(1:3:2), x
write(buf, fm(1:3:2)) x
if (trim(buf) /= '  1  2  3') error stop
end program

! The real and imaginary parts of a complex array are strided views
program print_arr_11
implicit none
complex :: z(3) = [(1.0, -1.0), (2.0, -2.0), (3.0, -3.0)]
complex, allocatable :: za(:,:)
character(len=60) :: buf
print *, z%re
print *, z%im
write(buf, '(3F5.1)') z%re
if (trim(buf) /= '  1.0  2.0  3.0') error stop 1
write(buf, '(3F5.1)') z%im
if (trim(buf) /= ' -1.0 -2.0 -3.0') error stop 2
allocate(za(0:1, 2))
za(0, 1) = (1.0, 5.0)
za(1, 1) = (2.0, 6.0)
za(0, 2) = (3.0, 7.0)
za(1, 2) = (4.0, 8.0)
write(buf, '(4F5.1)') za%im
if (trim(buf) /= '  5.0  6.0  7.0  8.0') error stop 3
write(buf, '(2F5.1)') za(1, :)%re
if (trim(buf) /= '  2.0  4.0') error stop 4
end program

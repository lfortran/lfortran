program array_section_c_stride_01
integer :: a(6), b(3), c(3), i
do i = 1, 6
  a(i) = i
end do
b = a(1:6:2)
c = a(6:1:-2)
if (b(1) /= 1 .or. b(2) /= 3 .or. b(3) /= 5) error stop
if (c(1) /= 6 .or. c(2) /= 4 .or. c(3) /= 2) error stop
end program

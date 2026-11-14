program select_case_c_range_01
integer :: x, i
integer :: hits(5)
hits = 0
do i = 1, 5
  x = i * 2 - 2   ! 0, 2, 4, 6, 8
  select case (x)
  case (1:3)
    hits(i) = 1
  case (4:6)
    hits(i) = 2
  case default
    hits(i) = 3
  end select
end do
if (hits(1) /= 3) error stop
if (hits(2) /= 1) error stop
if (hits(3) /= 2) error stop
if (hits(4) /= 2) error stop
if (hits(5) /= 3) error stop
end program

module gpu_metal_407_m
  implicit none
contains
  pure function total(a, k, masked) result(s)
    integer, intent(in) :: a(:), k
    logical, intent(in) :: masked
    integer :: s
    if (masked) then
      s = sum(a, mask=a > k)
    else
      s = sum(a) + k
    end if
  end function
end module

program gpu_metal_407
  use gpu_metal_407_m
  implicit none
  integer :: x(3), res(3), i
  x = [1, 2, 3]
  do concurrent (i = 1:3)
    res(i) = total(x, i, .false.)
  end do
  print *, res, total(x, 2, .true.)
  if (any(res /= [7, 8, 9])) error stop 1
  if (total(x, 2, .true.) /= 3) error stop 2
  if (total(x, 0, .true.) /= 6) error stop 3
end program

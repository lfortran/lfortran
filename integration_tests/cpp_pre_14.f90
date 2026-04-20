#define FOO(x,y) x + y

program cpp_pre_14
  implicit none
  integer, parameter :: FOO = 1
  integer :: r

  r = FOO(1,2) + FOO
  if (r /= 4) error stop
  print *, r
end program

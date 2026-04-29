program class_star_pointer_01
  implicit none
  class(*), pointer :: p(:)
  integer, target, allocatable :: a(:)
  allocate(a(-2:2))
  a = [10, 20, 30, 40, 50]
  p(-2:) => a
  select type (p)
  type is (integer)
     if (lbound(p,1) /= -2) error stop "lb"
     if (ubound(p,1) /= 2) error stop "ub"
     if (p(-2) /= 10) error stop "v1"
     if (p(2) /= 50) error stop "v2"
  class default
     error stop "wrong type"
  end select
  print *, "PASS"
end program

! Test: nested associate blocks with function calls inside a function
! called from do concurrent. Exercises gpu_offload pass symbol table
! fixup for FunctionCall nodes in duplicated associate block bodies.
module gpu_metal_150_m
  implicit none
contains
  pure function add_one(x) result(r)
    real, intent(in) :: x
    real :: r
    r = x + 1.0
  end function

  pure function compute(x) result(r)
    real, intent(in) :: x
    real :: r
    integer :: dummy
    associate(dummy => 1)
      associate(y => add_one(x))
        r = y + real(dummy)
      end associate
    end associate
  end function
end module

program gpu_metal_150
  use gpu_metal_150_m, only: compute
  implicit none
  integer, parameter :: n = 4
  real :: a(n), b(n)
  integer :: i
  do i = 1, n
    a(i) = real(i)
  end do
  do concurrent(i = 1:n)
    b(i) = compute(a(i))
  end do
  ! compute(x) = add_one(x) + 1 = (x+1) + 1 = x+2
  if (any(abs(b - [3.0, 4.0, 5.0, 6.0]) > 1.0e-6)) error stop
  print *, "ok"
end program

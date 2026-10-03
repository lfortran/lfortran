! Test: sum() on local array inside function called from do concurrent
! Verifies that Metal codegen generates correct address space overloads
! for lowered intrinsic functions (e.g. _lcompilers_Sum) so that
! thread-local arrays can be passed to them.
program gpu_metal_189
  implicit none
  real :: r(4)
  integer :: i

  do concurrent(i = 1:4)
    r(i) = f(real(i))
  end do

  print *, r(1)
  print *, r(2)
  print *, r(3)
  print *, r(4)

  if (abs(r(1) - 6.0) > 1e-5) error stop
  if (abs(r(2) - 9.0) > 1e-5) error stop
  if (abs(r(3) - 12.0) > 1e-5) error stop
  if (abs(r(4) - 15.0) > 1e-5) error stop

contains
  pure function f(x) result(s)
    real, intent(in) :: x
    real :: s
    real :: a(3)
    a(1) = x
    a(2) = x + 1.0
    a(3) = x + 2.0
    s = sum(a)
  end function
end program

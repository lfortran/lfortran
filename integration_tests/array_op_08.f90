program test 
  integer :: A(3) = [1,2,3]
  logical :: X(3)
  integer :: tol = 2
  real :: A2(3) = [1.0,2.0,3.0]
  real :: tol2 = 2.0
  integer :: i
  X = abs(A) <= max(tol, tol * maxval(abs(A)))
  print *, X
  do i = 1, 2, 3
    if (X(i) .neqv. .true.) error stop
  end do
  X = abs(A2) <= max(tol2, tol2 * maxval(abs(A2)))
  print *, X
  do i = 1, 2, 3
    if (X(i) .neqv. .true.) error stop
  end do
end program
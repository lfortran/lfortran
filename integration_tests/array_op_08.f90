program test 
  integer :: A(3) = [1,2,3]
  logical :: X(3)
  integer :: tol = 2
  integer :: i
  X = abs(A) <= max(tol, tol * maxval(abs(A)))
  do i = 1, 2, 3
    if (X(i) .neqv. .true.) error stop
  end do
end program
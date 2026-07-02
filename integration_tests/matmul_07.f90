program matmul_07
  implicit none

  logical :: a(2, 2)
  logical :: b(2, 2)
  logical :: c(2, 2)

  a = .true.
  b = .false.

  c = matmul(a, b)

  print *, c
  if (.not. all(c .eqv. .false.)) error stop
end program
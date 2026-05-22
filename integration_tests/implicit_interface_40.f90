program main
  implicit double precision (a-h, o-z)
  double precision :: x1(1), x2(1)

  call case5(x1, x2)

  if (abs(x1(1) - 0.23456d0) > 1d-12) error stop
  if (abs(x2(1) - 0.23456d0) > 1d-12) error stop
end program main

subroutine case5(x1, x2)
  implicit double precision (a-h, o-z)
  dimension x1(1), x2(1)
  external kern52, set51

  call perf(kern52, set51, x1, x2)
end subroutine case5

subroutine perf(kernel, setval, x1, x2)
  implicit double precision (a-h, o-z)
  external kernel, setval
  dimension x1(1), x2(1)

  call setval(x1, x2, s1)
  call kernel(x1, x2, s1)
end subroutine perf

subroutine set51(x1, x2, s1)
  implicit double precision (a-h, o-z)
  dimension x1(1), x2(1)

  s1 = 0.34567d0
  x1(1) = 0.12345d0
  x2(1) = 0.23456d0
end subroutine set51

subroutine kern52(x1, x2, s1)
  implicit double precision (a-h, o-z)
  dimension x1(1), x2(1)

  x1(1) = x2(1)
end subroutine kern52
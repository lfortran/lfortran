subroutine apply_scalar(f, y)
external f
real(8) :: y
call f(y)
end subroutine

subroutine apply_rhs(g, t, y, yp)
external g
real(8) :: t, y(2), yp(2)
call g(t, y, yp)
end subroutine

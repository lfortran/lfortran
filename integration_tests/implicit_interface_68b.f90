subroutine apply_sub(p, y)
external p
real(8) :: y
call p(y)
end subroutine

subroutine apply_fun(q, y)
real(8), external :: q
real(8) :: y
y = q(y)
end subroutine

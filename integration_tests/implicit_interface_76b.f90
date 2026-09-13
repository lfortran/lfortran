subroutine apply_76(p, a)
external p
real :: a(10)
call p(a)
end subroutine

subroutine other_76(x)
real :: x(10)
x(1) = x(1) + 10
end subroutine

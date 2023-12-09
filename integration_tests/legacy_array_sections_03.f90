DOUBLE PRECISION FUNCTION ddot_sl(dx)
DOUBLE PRECISION dx(*)
integer :: i
do i = 1, 4
    if (abs(dx(i) - 12.41d0) > 1e-10) error stop
end do
ddot_sl = dx(1)
END

SUBROUTINE slsqpb (n, a)
INTEGER n
external ddot_sl
DOUBLE PRECISION a(n+1), ddot_sl, res
print *, a
res = ddot_sl(a(1))
print *, res
if (abs(res - 12.41d0) > 1e-10) error stop
END

program legacy_array_sections_03
    double precision a(5)
    a = 12.41d0
    call slsqpb(4, a)
end program

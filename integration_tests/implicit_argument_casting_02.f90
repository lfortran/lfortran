subroutine idz_realcomp(n, a)
    implicit none
    integer n
    real a(n)
    a = 12.5d0
end subroutine


program main
implicit none
complex :: w(5)
call idz_realcomp(10, w(1))
print *, w
if (abs(real(w(1)) - 12.5) > 1e-6) error stop
end program

module associate_41_mod
implicit none
contains
subroutine sub()
    real :: f, x, result
    f(x) = x * 2.0
    associate (fnew => f(1.0))
        result = fnew
    end associate
    print *, result
    if (abs(result - 2.0) > 1e-6) error stop
end subroutine
end module associate_41_mod

program associate_41
use associate_41_mod, only: sub
call sub()
end program associate_41

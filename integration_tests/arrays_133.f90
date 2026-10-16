program arrays_133
! The external subroutines in arrays_133b.f90 and arrays_133c.f90 each get a
! getter for the use-associated bound n; the object files must link (#14542).
use arrays_133_mod, only: n
implicit none
interface
    subroutine arrays_133_sa(x)
        use arrays_133_mod, only: n
        real, intent(out) :: x(n)
    end subroutine
    subroutine arrays_133_sb(x, y)
        use arrays_133_mod, only: n
        real, intent(out) :: x(n), y(n + 1)
    end subroutine
end interface
real :: x(3), y(3), z(4)
call arrays_133_sa(x)
call arrays_133_sb(y, z)
print *, x, y, z
if (any(x /= 1)) error stop
if (any(y /= 2)) error stop
if (any(z /= 3)) error stop
end program

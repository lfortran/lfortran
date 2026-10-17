program separate_compilation_61
use separate_compilation_61a, only: t, impl
implicit none
interface
    subroutine separate_compilation_61_s(x, total)
        use separate_compilation_61a, only: t
        type(t) :: x
        real :: total
    end subroutine
end interface
type(t) :: x
real :: total
x%f => impl
call separate_compilation_61_s(x, total)
print *, total
if (abs(total - 28.0) > 1e-5) error stop
end program

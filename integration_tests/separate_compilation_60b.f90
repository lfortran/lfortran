module separate_compilation_60b
use separate_compilation_60a, only: s
implicit none
contains
    subroutine sb()
        real :: y(3)
        call s(y)
        print *, y
        if (any(y /= 6.0)) error stop
    end subroutine
end module

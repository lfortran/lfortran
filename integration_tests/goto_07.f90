module goto_07_m
implicit none
interface
    module subroutine set_value(i)
        integer, intent(out) :: i
    end subroutine set_value
end interface
end module goto_07_m

submodule(goto_07_m) goto_07_s
contains
module procedure set_value
i = 4
goto 40
i = 0
40 end procedure set_value
end submodule goto_07_s

program goto_07
use goto_07_m, only: set_value
implicit none
interface
    integer function f()
    end function f
end interface
integer :: i

i = 1
call increment(i)
if (i /= 2) error stop
if (f() /= 3) error stop
call set_value(i)
if (i /= 4) error stop
goto 10
error stop
10 end program goto_07

subroutine increment(i)
integer, intent(inout) :: i
i = i + 1
goto 20
error stop
20 end subroutine increment

integer function f()
f = 3
goto 30
f = 0
30 end function f

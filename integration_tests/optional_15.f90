! An `optional` statement that precedes the interface body of a dummy
! procedure must make that dummy procedure optional (issue #13495).
module optional_15_mod
implicit none
integer :: counter = 0
contains

integer function f(o, q) result(r)
integer, optional :: o
optional :: q
interface
subroutine q
end subroutine
end interface
r = 0
if (present(o)) r = o
if (present(q)) then
    call q()
    r = r + 100
end if
end function

subroutine g(p1, flag, p2)
optional :: p1
interface
subroutine p1
end subroutine
end interface
logical, intent(in), optional :: flag
optional :: p2
interface
subroutine p2
end subroutine
end interface
if (present(p1)) call p1()
if (present(flag)) then
    if (flag) counter = counter + 1000
end if
if (present(p2)) call p2()
end subroutine

subroutine bump
counter = counter + 1
end subroutine

end module

program optional_15
use optional_15_mod
implicit none
if (f(o=1) /= 1) error stop
if (f(2) /= 2) error stop
if (f() /= 0) error stop
if (counter /= 0) error stop
if (f(3, bump) /= 103) error stop
if (counter /= 1) error stop
if (f(q=bump) /= 100) error stop
if (counter /= 2) error stop

counter = 0
call g(bump, flag=.true.)
if (counter /= 1001) error stop
counter = 0
call g(flag=.true.)
if (counter /= 1000) error stop
counter = 0
call g()
if (counter /= 0) error stop
counter = 0
call g(p2=bump)
if (counter /= 1) error stop
counter = 0
call g(bump, .false., bump)
if (counter /= 2) error stop
print *, "ok"
end program

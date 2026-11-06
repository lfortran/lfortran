module modules_90_m
implicit none
type :: t
    integer :: v = 1
end type
end module
program modules_90
use modules_90_m, only: tt => t
implicit none
type(tt), pointer :: q
integer :: t
t = 5
q => null()
call s()
contains
subroutine s()
    associate(a => q)
    end associate
    if (t /= 5) error stop 1
    print *, "ok"
end subroutine
end program

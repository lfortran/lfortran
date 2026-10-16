! Procedure pointer components whose interface has a character result whose
! length calls a function, associated in another scope (#15004)
module proc_ptr_41_sizes
implicit none
contains
    pure integer function extent()
        extent = 2
    end function
end module

module proc_ptr_41_host
implicit none
type :: pp
    procedure(wide), pointer, nopass :: pw => null()
    procedure(later), pointer, nopass :: pl => null()
end type
contains
    pure integer function width()
        width = 3
    end function

    function wide(x) result(r)
        integer, intent(in) :: x
        character(len=width()) :: r
        r = "abc"
        if (x /= 7) r = "xyz"
    end function

    function later(x) result(r)
        use proc_ptr_41_sizes, only: extent
        integer, intent(in) :: x
        character(len=extent()) :: r
        r = "ab"
        if (x /= 7) r = "zz"
    end function

    subroutine invoke()
        type :: pq
            procedure(later), pointer, nopass :: pf => null()
        end type
        type(pq) :: a
        a = pq(later)
        if (len(a%pf(7)) /= 2) error stop
        if (a%pf(7) /= "ab") error stop
        if (a%pf(1) /= "zz") error stop
    end subroutine
end module

program proc_ptr_41
use proc_ptr_41_host
implicit none
type(pp) :: a
character(len=:), allocatable :: s
call invoke()
a%pw => wide
s = a%pw(7)
if (len(s) /= 3) error stop
if (s /= "abc") error stop
if (len(a%pw(1)) /= 3) error stop
if (a%pw(1) /= "xyz") error stop
a%pl => later
s = a%pl(7)
if (len(s) /= 2) error stop
if (s /= "ab") error stop
if (a%pl(1) /= "zz") error stop
a = pp(wide, later)
if (a%pw(1) /= "xyz") error stop
if (a%pl(1) /= "zz") error stop
print *, s
end program

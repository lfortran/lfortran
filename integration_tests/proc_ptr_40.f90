! A procedure pointer and a dummy procedure whose interface has a character
! result whose length calls a function use associated only in the interface
! (#15004)
module proc_ptr_40_sizes
implicit none
contains
    pure integer function extent()
        extent = 2
    end function
end module

module proc_ptr_40_host
implicit none
contains
    function f(x) result(r)
        use proc_ptr_40_sizes, only: extent
        integer, intent(in) :: x
        character(len=extent()) :: r
        r = "ab"
        if (x /= 7) r = "zz"
    end function

    subroutine call_dummy(fp)
        procedure(f) :: fp
        if (len(fp(7)) /= 2) error stop
        if (fp(7) /= "ab") error stop
        if (fp(1) /= "zz") error stop
    end subroutine
end module

program proc_ptr_40
use proc_ptr_40_host
implicit none
procedure(f), pointer :: q
character(len=:), allocatable :: s
q => f
s = q(7)
if (len(s) /= 2) error stop
if (s /= "ab") error stop
if (len(q(1)) /= 2) error stop
if (q(1) /= "zz") error stop
call call_dummy(f)
call call_dummy(q)
print *, s
end program

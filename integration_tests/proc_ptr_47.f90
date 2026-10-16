! A procedure pointer component whose interface is an internal procedure
! with a character result whose length calls a function use associated only
! in that procedure (#15004)
module proc_ptr_47_sizes
implicit none
contains
    pure integer function extent()
        extent = 2
    end function
end module

program proc_ptr_47
implicit none
type :: pp
    procedure(later), pointer, nopass :: pf => null()
end type
type(pp) :: a
character(len=:), allocatable :: s
! A local of the host, unrelated to the `extent` in the result length
integer :: extent
extent = 5
a%pf => later
s = a%pf(7)
if (len(s) /= 2) error stop
if (s /= "ab") error stop
if (len(a%pf(1)) /= 2) error stop
if (a%pf(1) /= "zz") error stop
if (later(1) /= "zz") error stop
if (extent /= 5) error stop
print *, s
contains
    function later(x) result(r)
        use proc_ptr_47_sizes, only: extent
        integer, intent(in) :: x
        character(len=extent()) :: r
        r = "ab"
        if (x /= 7) r = "zz"
    end function
end program

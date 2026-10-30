module procedure_bounds_02_m
implicit none
abstract interface
    integer function callback(values)
        integer, intent(in) :: values(3)
    end function
end interface
contains
integer function first(values)
    integer, intent(in) :: values(3)
    first = values(1)
end function
subroutine invoke(f, values)
    procedure(callback) :: f
    integer, allocatable, intent(in) :: values(:)
    print *, f(values)
end subroutine
end module
program procedure_bounds_02
use procedure_bounds_02_m
implicit none
integer, allocatable :: values(:)
allocate(values(2))
values = 0
call invoke(first, values)
print *, "missing callback argument bounds check"
end program

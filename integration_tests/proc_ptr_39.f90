module proc_ptr_39_sizes
implicit none
integer :: k = 3
contains
    pure integer function extent()
        extent = 2
    end function
end module

module proc_ptr_39_iface
implicit none
contains
    integer function total(x)
        use proc_ptr_39_sizes, only: extent
        integer, intent(in) :: x(extent())
        total = sum(x)
    end function

    integer function first(y)
        use proc_ptr_39_sizes, only: k
        integer, intent(in) :: y(k)
        first = y(1)
    end function
end module

module proc_ptr_39_host
use proc_ptr_39_iface, only: total, first
implicit none
type :: pp
    procedure(total), pointer, nopass :: pf => null()
    procedure(first), pointer, nopass :: pg => null()
end type
type(pp), save :: g = pp()
type(pp), save :: h = pp(null(), null())
end module

program proc_ptr_39
use proc_ptr_39_host
use proc_ptr_39_iface, only: total, first
implicit none
type(pp) :: a = pp()
type(pp) :: b = pp(pg=null())
if (associated(g%pf) .or. associated(g%pg)) error stop
if (associated(h%pf) .or. associated(h%pg)) error stop
if (associated(a%pf) .or. associated(a%pg)) error stop
if (associated(b%pf) .or. associated(b%pg)) error stop
g%pf => total
g%pg => first
if (g%pf([40, 2]) /= 42) error stop
if (g%pg([7, 8, 9]) /= 7) error stop
b%pf => total
if (b%pf([5, 6]) /= 11) error stop
print *, "ok"
end program

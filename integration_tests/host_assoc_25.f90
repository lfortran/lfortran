! A host declared NON_RECURSIVE is entered again while an instance that made
! a procedure value of its internal procedure is active (which the standard
! does not allow): the second instance stops with an error that says that
! the host is not recursive.
module host_assoc_25_m
implicit none
abstract interface
    integer function getter()
    end function
end interface
contains
non_recursive function host(depth, older) result(r)
    integer, intent(in) :: depth
    procedure(getter) :: older
    integer :: r
    r = older()
    if (depth > 1) r = r + reenter(depth - 1, get)
contains
    integer function get()
        get = depth
    end function
end function

recursive integer function reenter(depth, older)
    integer, intent(in) :: depth
    procedure(getter) :: older
    reenter = host(depth, older)
end function

integer function zero()
    zero = 0
end function
end module

program host_assoc_25
use host_assoc_25_m
implicit none
print *, host(3, zero)
end program

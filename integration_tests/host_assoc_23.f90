! More instances of a recursive host hold a procedure value of its internal
! procedure at the same time than LFortran is asked for thunks
! (--internal-procedure-thunks=2): each instance passes its own `get` to the
! next one, and the third instance that does stops with an error that names
! the option.
module host_assoc_23_m
implicit none
abstract interface
    integer function getter()
    end function
end interface
contains
recursive integer function host(depth, older) result(r)
    integer, intent(in) :: depth
    procedure(getter) :: older
    r = older()
    if (depth > 1) r = r + host(depth - 1, get)
contains
    integer function get()
        get = depth
    end function
end function

integer function zero()
    zero = 0
end function
end module

program host_assoc_23
use host_assoc_23_m
implicit none
print *, host(4, zero)
end program

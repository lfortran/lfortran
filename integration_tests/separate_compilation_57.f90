program separate_compilation_57
! A function imported from a separately compiled module that builds an
! extended type whose only component is inherited from a parent type
! declared in another module (#14339)
use separate_compilation_57a, only: create
implicit none
integer :: m

m = get_n()
print *, m
if (m /= 7) error stop

contains

    integer function get_n()
        use separate_compilation_57a, only: child_t
        type(child_t) :: c
        c = create()
        get_n = c%n
    end function
end program

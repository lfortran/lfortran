! Error: in an internal procedure, a local variable hides the host's
! module entity of the same name, so "m%x" refers to a component of an integer.
module namespace_modules_21_m
    implicit none
    integer :: x = 1
end module

program namespace_modules_21
    use, namespace :: m => namespace_modules_21_m
    implicit none
    call sub()
contains
    subroutine sub()
        integer :: m
        m = 2
        print *, m%x
    end subroutine
end program

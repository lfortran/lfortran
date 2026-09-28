! Error: a namespace cannot be passed as an actual argument.
module namespace_modules_07_m
    implicit none
    integer :: x = 1
end module

program namespace_modules_07
    use, namespace :: m => namespace_modules_07_m
    implicit none
    call show(m)
contains
    subroutine show(a)
        integer, intent(in) :: a
        print *, a
    end subroutine
end program

module xx

    type :: base
        integer :: x
    contains
        procedure, non_overridable :: show_x
    end type

    contains
        subroutine show_x(this)
            class(base), intent(inout) :: this
            this%x = 12
        end subroutine show_x
end module

program main
    use xx
    implicit none

    type(base) :: b
    b%x = 10
    call b%show_x()
    print *, b%x
    if ( b%x /= 12 ) error stop
end program


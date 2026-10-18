! `t%x = h()`, where the internal function h writes another component of the
! allocatable host variable t (#14849)
module nested_vars_24_mod
    implicit none
    type :: a_t
        integer :: x, y
    end type
contains
    subroutine s()
        type(a_t), allocatable :: t
        allocate(t)
        t%x = 1
        t%y = 10
        t%x = h()
        if (t%x /= 5) error stop 1
        if (t%y /= 2) error stop 2
        print *, t%x, t%y
    contains
        integer function h()
            t%y = 2
            h = 5
        end function
    end subroutine
end module

program nested_vars_24
    use nested_vars_24_mod
    implicit none
    call s()
end program

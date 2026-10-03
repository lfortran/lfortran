module modules_75_a
    implicit none
    type :: point
        integer :: x = 0
    end type point
    integer :: counter = 0
contains
    subroutine bump()
        counter = counter + 1
    end subroutine bump
end module modules_75_a

module modules_75_b
    implicit none
contains
    subroutine twice()
        ! Imports of a procedure of its own.
        use modules_75_a, only: bump
        call bump()
        call bump()
    end subroutine twice

    integer function get_x()
        use modules_75_a, only: point
        type(point) :: p
        p%x = 7
        get_x = p%x
    end function get_x
end module modules_75_b

subroutine modules_75_ext()
    use modules_75_a, only: bump, counter
    implicit none
    call bump()
    counter = counter + 10
end subroutine modules_75_ext

program modules_75
    use modules_75_a, only: counter
    use modules_75_b, only: twice, get_x
    implicit none
    call twice()
    call modules_75_ext()
    if (counter /= 13) error stop
    if (get_x() /= 7) error stop
    print *, counter, get_x()
end program modules_75

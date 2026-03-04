! Test: derived type array with allocatable component in block scope.
! The allocatable components must be finalized before freeing the array.
program block_13
    implicit none
    type :: t
        character(len=:), allocatable :: c
    end type

    block
        type(t) :: x(1)
        x(1)%c = "hello"
        if (x(1)%c /= "hello") error stop
    end block

    block
        type(t) :: y(3)
        y(1)%c = "a"
        y(2)%c = "bb"
        y(3)%c = "ccc"
        if (len(y(3)%c) /= 3) error stop
    end block

    print *, "ok"
end program block_13

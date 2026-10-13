program enum_09
    implicit none
    integer :: red, k
    red = 10

    ! The issue's code: enumerators with explicit values
    block
        enum, bind(c)
            enumerator :: red = 1, green = 2
        end enum
        if (red /= 1) error stop
        if (green /= 2) error stop
        print *, red, green
    end block
    ! The enumerator `red` hid the host variable only inside the BLOCK
    if (red /= 10) error stop

    ! Implicit and mixed enumerator values, used in a specification
    ! expression and in a nested BLOCK
    block
        enum, bind(c)
            enumerator :: c0, c1, c5 = 5, c6
        end enum
        integer :: arr(c6)
        if (c0 /= 0) error stop
        if (c1 /= 1) error stop
        if (c5 /= 5) error stop
        if (c6 /= 6) error stop
        if (size(arr) /= 6) error stop
        block
            if (c6 - c0 /= 6) error stop
        end block
    end block

    ! Two enums in one named BLOCK, inside a loop, used in SELECT CASE
    k = 0
    do red = 1, 2
        named: block
            enum, bind(c)
                enumerator :: north = 1, south
            end enum
            enum, bind(c)
                enumerator :: east = 10, west
            end enum
            select case (red)
            case (north)
                k = k + east
            case (south)
                k = k + west
            end select
        end block named
    end do
    if (k /= 21) error stop

    if (in_sub() /= 7) error stop

contains

    integer function in_sub() result(r)
        block
            enum, bind(c)
                enumerator :: a = 3, b
            end enum
            r = a + b
        end block
    end function

end program

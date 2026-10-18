module bindc_75_mod
    use iso_c_binding, only: c_char
    implicit none
contains
    subroutine bindc_75_explicit(a) bind(c)
        character(kind=c_char), intent(inout) :: a(-1:1)
        if (a(-1) /= 'x') error stop 1
        a(1) = a(-1)
        a(-1) = 'a'
    end subroutine

    subroutine bindc_75_default(a) bind(c)
        character, intent(inout) :: a(3)
        if (a(1) /= 'x') error stop 2
        a(3) = a(1)
        a(1) = 'a'
    end subroutine

    subroutine bindc_75_matrix(a) bind(c)
        character(kind=c_char), intent(inout) :: a(0:1, -1:0)
        if (a(1, -1) /= 'b') error stop 3
        a(1, 0) = a(0, 0)
        a(0, 0) = 'X'
    end subroutine

    subroutine bindc_75_single(a) bind(c)
        character(kind=c_char), intent(out) :: a(1)
        a(1) = 's'
    end subroutine

    subroutine bindc_75_assumed(a) bind(c)
        character(kind=c_char), intent(inout) :: a(*)
        a(2) = a(1)
    end subroutine

    subroutine bindc_75_scalar(a) bind(c)
        character(kind=c_char), intent(out) :: a
        a = 't'
    end subroutine
end module

program bindc_75
    use iso_c_binding, only: c_int
    use bindc_75_mod
    implicit none
    character(kind=c_char) :: a(3), b(2, 2), single(1), scalar
    interface
        integer(c_int) function bindc_75_check() bind(c)
            import :: c_int
        end function
    end interface

    a = ['x', 'y', 'z']
    call bindc_75_explicit(a)
    if (a(1) /= 'a' .or. a(2) /= 'y' .or. a(3) /= 'x') error stop 4
    a = ['x', 'y', 'z']
    call bindc_75_default(a)
    if (a(1) /= 'a' .or. a(2) /= 'y' .or. a(3) /= 'x') error stop 5
    b(1, 1) = 'a'
    b(2, 1) = 'b'
    b(1, 2) = 'c'
    b(2, 2) = 'd'
    call bindc_75_matrix(b)
    if (b(1, 1) /= 'a' .or. b(2, 1) /= 'b') error stop 6
    if (b(1, 2) /= 'X' .or. b(2, 2) /= 'c') error stop 7
    call bindc_75_single(single)
    if (single(1) /= 's') error stop 8
    call bindc_75_assumed(a)
    if (a(1) /= 'a' .or. a(2) /= 'a' .or. a(3) /= 'x') error stop 9
    call bindc_75_scalar(scalar)
    if (scalar /= 't') error stop 10
    if (bindc_75_check() /= 0) error stop 11
end program

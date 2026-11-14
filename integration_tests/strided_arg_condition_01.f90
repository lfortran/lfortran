module strided_arg_condition_01_m
    implicit none
    type :: box
        integer :: y
    end type
contains
    logical function inc(x)
        integer, intent(inout) :: x(:)
        x = x + 1
        inc = all(x < 4)
    end function
    integer function next(x)
        integer, intent(inout) :: x(:)
        x = x + 1
        next = x(1)
    end function
end module

program strided_arg_condition_01
    ! The copy back of b%y, an argument of a function in the head of an IF,
    ! DO WHILE, SELECT CASE or DO construct, must happen before the body runs.
    use strided_arg_condition_01_m
    implicit none
    type(box) :: b(3)
    integer :: n, k
    b%y = 0
    if (inc(b%y)) then
        if (any(b%y /= 1)) error stop
    end if
    n = 0
    do while (inc(b%y))
        n = n + 1
        if (any(b%y /= n + 1)) error stop
    end do
    if (n /= 2 .or. any(b%y /= 4)) error stop
    select case (next(b%y))
    case (5)
        if (any(b%y /= 5)) error stop
    case default
        error stop
    end select
    n = 0
    do k = 1, next(b%y) - 4
        n = n + 1
        if (any(b%y /= 6)) error stop
    end do
    if (n /= 2) error stop
    print *, b%y
end program

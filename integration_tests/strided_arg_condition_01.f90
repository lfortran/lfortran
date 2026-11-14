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
end module

program strided_arg_condition_01
    ! The copy back of b%y, an argument of a function in an IF or DO WHILE
    ! condition, must happen before the body runs.
    use strided_arg_condition_01_m
    implicit none
    type(box) :: b(3)
    integer :: n
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
    print *, b%y
end program

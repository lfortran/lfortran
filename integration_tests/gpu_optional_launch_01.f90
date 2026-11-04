module gpu_optional_launch_01_m
    implicit none
contains
    subroutine project(x, y, bias)
        real, contiguous, intent(in) :: x(:)
        real, contiguous, intent(out) :: y(:)
        real, contiguous, optional, intent(in) :: bias(:)
        real :: work(size(x))
        integer :: i
        do concurrent (i = 1:size(x))
            work(i) = 2*x(i)
        end do
        do concurrent (i = 1:size(y))
            block
                real :: total
                total = work(i)
                if (present(bias)) total = total + bias(i)
                y(i) = total
            end block
        end do
    end subroutine

    pure real function evaluate(x, bias)
        real, intent(in) :: x
        real, optional, intent(in) :: bias
        evaluate = 3*x
        if (present(bias)) evaluate = evaluate + bias
    end function

    subroutine forward(x, y, bias)
        real, intent(in) :: x(:)
        real, intent(out) :: y(:)
        real, optional, intent(in) :: bias
        integer :: i
        do concurrent (i = 1:size(x))
            y(i) = evaluate(x(i), bias)
        end do
    end subroutine

    subroutine update(x, first, bias)
        real, intent(in) :: x(:)
        real, intent(out) :: first
        real, optional, intent(inout) :: bias(:)
        integer :: i
        do concurrent (i = 1:size(x))
            if (present(bias)) bias(i) = bias(i) + x(i)
        end do
        first = -1
        if (present(bias)) then
            first = bias(1)
            bias(1) = first + 10
        end if
    end subroutine

    subroutine check(n)
        integer, intent(in) :: n
        real :: x(n), y(n), bias(n), first
        integer :: i
        do i = 1, n
            x(i) = real(i)
            bias(i) = real(n-i)
        end do
        y = -1
        call project(x, y)
        if (any(y /= 2*x)) error stop 1
        call project(x, y, bias)
        if (any(y /= 2*x + bias)) error stop 2
        x = x + 1
        call project(x, y)
        if (any(y /= 2*x)) error stop 3

        y = -1
        call forward(x, y)
        if (any(y /= 3*x)) error stop 4
        call forward(x, y, 7.)
        if (any(y /= 3*x + 7)) error stop 5
        x = x + 2
        call forward(x, y)
        if (any(y /= 3*x)) error stop 6

        call update(x, first)
        if (first /= -1) error stop 8
        bias = 1
        call update(x, first, bias)
        if (first /= 1 + x(1)) error stop 9
        if (bias(1) /= 11 + x(1)) error stop 10
        if (any(bias(2:) /= 1 + x(2:))) error stop 11
        call update(x, first)
        if (first /= -1) error stop 12
    end subroutine
end module

module gpu_optional_record_01_m
    implicit none
    type :: record
        integer :: v
    end type
contains
    subroutine calculate(n, got, count, a)
        integer, intent(in) :: n
        integer, intent(out) :: got, count
        type(record), optional, intent(inout) :: a(n)
        integer :: i, required(n)
        do concurrent (i = 1:n)
            required(i) = i
            if (present(a)) a(i)%v = a(i)%v + i
        end do
        got = -2
        if (present(a)) then
            got = a(1)%v
            a(1)%v = got + 20
        end if
        count = required(n)
    end subroutine

    subroutine check_record(n)
        integer, intent(in) :: n
        type(record) :: a(n)
        integer :: got, count, i
        do i = 1, n
            a(i)%v = 10
        end do
        call calculate(n, got, count)
        if (got /= -2 .or. count /= n) error stop 13
        call calculate(n, got, count, a)
        print *, "aggregate observed", got, a(1)%v, count
        if (got /= 11 .or. count /= n) error stop 14
        if (a(1)%v /= 31) error stop 15
        do i = 2, n
            if (a(i)%v /= 10 + i) error stop 16
        end do
        call calculate(n, got, count)
        if (got /= -2 .or. count /= n) error stop 17
    end subroutine
end module

program gpu_optional_launch_01
    use gpu_optional_launch_01_m
    use gpu_optional_record_01_m
    implicit none
    integer :: n
    character(16) :: arg
    n = 37
    call get_command_argument(1, arg)
    if (len_trim(arg) /= 0) read(arg, *) n
    if (n < 1 .or. n > 1000) error stop 7
    call check(n)
    call check_record(n)
    print *, "optional launches passed", n
end program

module stack_array_function_temporaries
    implicit none
    integer :: bound_calls = 0
contains
    function extent(n) result(k)
        integer, intent(in) :: n
        integer :: k
        bound_calls = bound_calls + 1
        k = n
    end function

    pure function evaluate(x) result(y)
        real, intent(in) :: x(:)
        real :: y(-2:size(x)-3)
        y = 1.0 / (1.0 + exp(-x))
    end function

    function sized(n) result(y)
        integer, intent(in) :: n
        real :: y(n)
        y = 2.0
    end function

    function allocated_result(n) result(y)
        integer, intent(in) :: n
        real, allocatable :: y(:)
        allocate(y(-3:n-4))
        y = 3.0
    end function

    function changing_extent(n) result(y)
        integer, intent(inout) :: n
        real :: y(n)
        y = 4.0
        n = n+1
    end function

    subroutine check_automatic(n)
        integer, intent(in) :: n
        real :: x(n)
        x = 2.0
        if (any(x /= 2.0)) error stop 9
    end subroutine

    subroutine check(skip, early)
        logical, intent(in) :: skip
        logical, intent(in) :: early
        real :: x(-128:127), y(256), expected(256)
        real, allocatable :: a(:)
        integer :: i, n, divisor, before
        do i = -128, 127
            x(i) = real(i) * 0.125
        end do
        divisor = 0
        if (.not. skip) then
            ! The result's metadata must not be evaluated on a skipped path.
            y(1:1) = sized(1/divisor) + 1.0
        end if
        do i = 1, 20000
            n = mod(i, 257)
            y(1:n) = evaluate(x(-128:n-129)) * (1.0-evaluate(x(-128:n-129)))
            expected(1:n) = 1.0/(1.0+exp(-x(-128:n-129)))
            expected(1:n) = expected(1:n)*(1.0-expected(1:n))
            if (any(abs(y(1:n)-expected(1:n)) > 1.e-6)) error stop 1
            if (early .and. i == 31) return
            if (mod(i, 7) == 0) cycle
            y(1:11) = evaluate(x(10:-10:-2)) + evaluate(x(-10:10:2))
            if (any(abs(y(1:11)-1.0) > 1.e-6)) error stop 2
        end do
        before = bound_calls
        n = 5
        y(1:n) = sized(extent(n)) + 1.0
        if (bound_calls /= before+1) error stop 3
        if (any(y(1:n) /= 3.0)) error stop 4
        y(1:5) = changing_extent(n) + 1.0
        if (n /= 6 .or. any(y(1:5) /= 5.0)) error stop 8
        n = 5
        a = allocated_result(n)
        if (.not. allocated(a)) error stop 5
        if (lbound(a, 1) /= 1 .or. size(a) /= n) error stop 6
        if (any(a /= 3.0)) error stop 7
        return
    end subroutine
end module

program main
    use stack_array_function_temporaries, only: check, check_automatic
    implicit none
    call check(.true., .false.)
    call check_automatic(17)
    call check(.true., .true.)
    call check(.true., .false.)
end program

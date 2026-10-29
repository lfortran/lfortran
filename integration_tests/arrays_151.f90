module arrays_151_values
    implicit none
    integer :: calls
contains
    function reverse_host(values) result(x)
        real(8), intent(in) :: values(-1:1)
        real(8) :: x(3)
        calls = calls + 1
        x(1) = values(1)
        x(2) = values(0)
        x(3) = values(-1)
    end function

end module

program arrays_151
    use arrays_151_values
    implicit none
    real(8) :: a(-1:1), b(-1:0,3:4)
    integer :: repeat, n

    calls = 0
    do repeat = 1, 3
        a(-1) = 1.0_8
        a(0) = 2.0_8
        a(1) = 3.0_8
        a = reverse_host(a)
        if (a(-1) /= 3.0_8 .or. a(0) /= 2.0_8 .or. a(1) /= 1.0_8) error stop
        if (calls /= 3 * repeat - 2) error stop
        a = reverse_host(a) + reverse_host(a)
        if (a(-1) /= 2.0_8 .or. a(0) /= 4.0_8 .or. a(1) /= 6.0_8) error stop
        if (calls /= 3 * repeat) error stop
        b = matrix()
        if (b(-1,3) /= 1.0_8 .or. b(0,3) /= 2.0_8) error stop
        if (b(-1,4) /= 3.0_8 .or. b(0,4) /= 4.0_8) error stop
        do n = 0, 3
            call check(vector(n), n)
        end do
        if (b(-1,3) /= 1.0_8 .or. a(-1) /= 2.0_8) error stop
    end do
contains
    function matrix() result(x)
        real(8) :: x(2,2)
        x(1,1) = 1.0_8
        x(2,1) = 2.0_8
        x(1,2) = 3.0_8
        x(2,2) = 4.0_8
    end function

    function vector(n) result(x)
        integer, intent(in) :: n
        real(8) :: x(n)
        integer :: i
        do i = 1, n
            x(i) = i * 1.0_8
        end do
    end function

    subroutine check(x, n)
        integer, intent(in) :: n
        real(8), intent(in) :: x(:)
        integer :: i
        if (size(x) /= n) error stop
        do i = 1, n
            if (x(i) /= i * 1.0_8) error stop
        end do
    end subroutine
end program

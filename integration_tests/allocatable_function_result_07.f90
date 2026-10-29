module allocatable_function_result_07_m
    implicit none
    abstract interface
        function maker(n) result(r)
            integer, intent(in) :: n
            integer, allocatable :: r(:)
        end function
    end interface
contains
    function values(n) result(r)
        integer, intent(in) :: n
        integer, allocatable :: r(:)
        integer :: i
        allocate(r(n))
        do i = 1, n
            r(i) = 10*i
        end do
    end function

    subroutine invoke(f, n)
        procedure(maker) :: f
        integer, intent(in) :: n
        call check(f(n), n)
    end subroutine

    subroutine check(a, n)
        integer, intent(in) :: a(:), n
        integer :: i
        if (size(a) /= n) error stop 1
        do i = 1, n
            if (a(i) /= 10*i) error stop 2
        end do
    end subroutine
end module

program allocatable_function_result_07
    use allocatable_function_result_07_m
    implicit none
    integer :: n
    do n = 0, 4
        call invoke(values, n)
    end do
end program

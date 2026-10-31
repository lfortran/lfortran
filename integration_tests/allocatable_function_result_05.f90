program allocatable_function_result_05
    implicit none
    call check(make_result())
contains
    function make_result() result(r)
        integer, allocatable :: r(:)
        allocate(r(1))
        r(1) = 17
    end function

    subroutine check(a)
        integer, intent(in) :: a(:)
        if (size(a) /= 1) error stop 1
        if (a(1) /= 17) error stop 2
    end subroutine
end program

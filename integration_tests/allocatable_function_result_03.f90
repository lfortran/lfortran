! An allocatable scalar function result in a specification expression.
module allocatable_function_result_03_m
    implicit none
contains
    pure function next(k) result(r)
        integer, intent(in) :: k
        integer, allocatable :: r
        r = k + 1
    end function

    subroutine check(n)
        integer, intent(in) :: n
        character(len=next(n)) :: s
        real :: a(next(n) + 1)
        if (len(s) /= n + 1) error stop 1
        if (size(a) /= n + 2) error stop 2
    end subroutine
end module

program allocatable_function_result_03
    use allocatable_function_result_03_m
    implicit none
    call check(4)
    call check(0)
    print *, "ok"
end program

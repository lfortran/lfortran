! Intrinsic assignment of an allocatable scalar function result to an
! allocated variable of the same type keeps the allocation of the variable
! (F2018 10.2.1.3), so a pointer associated with it stays associated
! (#15640).
module allocatable_function_result_02_m
    implicit none
contains
    function twice(x) result(r)
        real(8), intent(in) :: x
        real(8), allocatable :: r
        r = 2*x
    end function

    function next(k) result(r)
        integer, intent(in) :: k
        integer, allocatable :: r
        r = k + 1
    end function
end module

program allocatable_function_result_02
    use allocatable_function_result_02_m
    implicit none
    real(8), allocatable, target :: v
    real(8), pointer :: p
    integer, allocatable, target :: n
    integer, pointer :: q
    integer :: i

    v = 3
    p => v
    v = twice(1d0)
    if (abs(v - 2) > 1d-12) error stop 1
    p = 5
    if (abs(v - 5) > 1d-12) error stop 2
    print *, v

    v = twice(v)
    if (abs(p - 10) > 1d-12) error stop 3

    n = 0
    q => n
    do i = 1, 10
        n = next(n)
    end do
    if (n /= 10) error stop 4
    q = -1
    if (n /= -1) error stop 5
    print *, n
end program

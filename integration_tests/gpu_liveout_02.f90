! A scalar a `do concurrent` defines in one iteration keeps that value
! (Fortran 2018, 11.1.7.5) when it outlives the procedure holding the
! loop -- a dummy argument, the function result, a module variable, a
! variable of the host procedure -- or a statement around the loop reads
! it afterwards; a dummy argument that every iteration assigns and then
! reads is still private to each iteration.
module gpu_liveout_02_m
    implicit none
    integer :: m
contains
    subroutine find(n, x, k)
        integer, intent(in) :: n
        real, intent(in) :: x(n)
        integer, intent(out) :: k
        integer :: i
        k = 0
        do concurrent (i = 1:n)
            if (x(i) < 0.0) k = i
        end do
    end subroutine

    subroutine set_module(n)
        integer, intent(in) :: n
        integer :: i
        do concurrent (i = 1:n)
            if (i == n) m = 3 * i
        end do
    end subroutine

    integer function second(n) result(r)
        integer, intent(in) :: n
        integer :: i
        r = 0
        do concurrent (i = 1:n)
            if (i == 2) r = 10 * i
        end do
    end function

    subroutine host(n, k)
        integer, intent(in) :: n
        integer, intent(out) :: k
        k = 0
        call inner()
    contains
        subroutine inner()
            integer :: i
            do concurrent (i = 1:n)
                if (i == 3) k = i
            end do
        end subroutine
    end subroutine

    subroutine temp(n, a, b, t)
        integer, intent(in) :: n
        real, intent(in) :: a(n)
        real, intent(out) :: b(n)
        real, intent(inout) :: t
        integer :: i
        do concurrent (i = 1:n)
            t = 2 * a(i)
            b(i) = t + 1
        end do
    end subroutine
end module

program gpu_liveout_02
    use gpu_liveout_02_m
    implicit none
    real :: x(100), b(100), t
    integer :: k, j
    logical :: flag
    x = 1.0
    x(42) = -1.0
    call find(100, x, k)
    print *, k
    if (k /= 42) error stop 1
    m = 0
    call set_module(5)
    if (m /= 15) error stop 2
    if (second(5) /= 20) error stop 3
    call host(5, k)
    if (k /= 3) error stop 4
    x = [(real(j), j = 1, 100)]
    t = 0
    call temp(100, x, b, t)
    do j = 1, 100
        if (b(j) /= 2 * j + 1) error stop 5
    end do
    flag = .true.
    k = 5
    if (flag) then
        do concurrent (j = 1:3)
            if (j == 1) k = 7
        end do
    end if
    print *, k
    if (k /= 7) error stop 6
end program

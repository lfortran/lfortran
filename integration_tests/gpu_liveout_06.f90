! A variable that every iteration of a `do concurrent` passes to a
! procedure that may define it, which defines it in one iteration only:
! a scalar and a derived-type variable passed to `intent(inout)` dummy
! arguments, and a scalar passed to an `intent(out)` one. Each thread
! puts its copy back only when the call changed it (or whole, for
! `intent(out)`), so that the threads that leave it alone do not put back
! the value they read on entry over the one the defining iteration gave
! it.
module gpu_liveout_06_m
    implicit none
    type :: pt
        integer :: x, y
    end type
contains
    pure subroutine maybe_set(k, i)
        integer, intent(inout) :: k
        integer, intent(in) :: i
        if (i == 7) k = 70
    end subroutine

    pure subroutine maybe_move(p, i)
        type(pt), intent(inout) :: p
        integer, intent(in) :: i
        if (i == 9) p%y = 90
    end subroutine

    pure subroutine set(k, i)
        integer, intent(out) :: k
        integer, intent(in) :: i
        k = 2 * i
    end subroutine
end module

program gpu_liveout_06
    use gpu_liveout_06_m
    implicit none
    integer, parameter :: n = 100000
    integer :: k, m, i
    type(pt) :: p
    integer, allocatable :: a(:)
    allocate(a(n))
    k = 0
    p = pt(1, 2)
    do concurrent (i = 1:n)
        a(i) = i
        call maybe_set(k, i)
        call maybe_move(p, i)
    end do
    print *, k, p%x, p%y
    if (k /= 70) error stop 1
    if (p%x /= 1 .or. p%y /= 90) error stop 2
    m = 0
    do concurrent (i = 1:n)
        if (i == n) call set(m, i)
    end do
    print *, m
    if (m /= 2 * n) error stop 3
end program

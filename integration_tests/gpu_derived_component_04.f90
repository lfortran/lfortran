! A derived-type variable with nothing allocatable or a pointer in it that
! every iteration of a `do concurrent` assigns whole, or a component of a
! component of, and then reads is a temporary of the iteration (Fortran
! 2018, 11.1.7.5), whether it is a dummy argument or a local the host
! reads after the loop; what one iteration alone defines of it, a
! component of a component included, is kept.
module gpu_derived_component_04_m
    implicit none
    type :: point
        real :: x, y
    end type
    type :: outer
        type(point) :: a
        integer :: k
    end type
contains
    subroutine s(p, x, y, r)
        type(point), intent(inout) :: p
        real, intent(in) :: x(:), y(:)
        real, intent(out) :: r(:)
        integer :: i
        do concurrent (i = 1:size(r))
            p = point(x(i), y(i))
            r(i) = p%x + p%y
        end do
    end subroutine

    subroutine t(o, a)
        type(outer), intent(inout) :: o
        integer, intent(out) :: a(:)
        integer :: i
        do concurrent (i = 1:size(a))
            o%a%x = i
            a(i) = nint(o%a%x)
            if (i == 3) o%k = 7
            if (i == 5) o%a%y = 50
        end do
    end subroutine
end module

program gpu_derived_component_04
    use gpu_derived_component_04_m
    implicit none
    integer, parameter :: n = 100000
    type(point) :: p, q, last
    type(outer) :: o
    real, allocatable :: x(:), y(:), r(:)
    integer, allocatable :: a(:)
    integer :: i
    allocate(x(n), y(n), r(n), a(n))
    do i = 1, n
        x(i) = i
        y(i) = 2 * i
    end do
    p = point(0, 0)
    call s(p, x, y, r)
    do i = 1, n
        if (r(i) /= 3 * i) error stop 1
    end do
    q = point(0, 0)
    last = point(0, 0)
    do concurrent (i = 1:n)
        q = point(y(i), x(i))
        r(i) = q%x - q%y
        if (i == n) last = q
    end do
    do i = 1, n
        if (r(i) /= i) error stop 2
    end do
    if (last%x /= 2 * n .or. last%y /= n) error stop 3
    o%a = point(0, 0)
    o%k = 0
    call t(o, a)
    do i = 1, n
        if (a(i) /= i) error stop 4
    end do
    if (o%k /= 7) error stop 5
    if (o%a%y /= 50) error stop 7
    q = point(1, 2)
    if (q%y /= 2) error stop 6
    print *, r(1), r(n), last%x, last%y, a(n), o%k, q%y
end program

! A derived-type variable, or a scalar component of one, that one
! iteration of a `do concurrent` defines by passing it to a procedure that
! defines it keeps that value (Fortran 2018, 11.1.7.5), and one that every
! iteration defines that way and then reads is a temporary of the
! iteration; writes to an allocatable array component reach it.
module gpu_derived_component_05_m
    implicit none
    type :: pt
        integer :: x, y
    end type
    type :: u
        real, allocatable :: w(:)
        integer :: k
    end type
contains
    pure subroutine bump(k)
        integer, intent(inout) :: k
        k = k + 1
    end subroutine

    pure subroutine setp(q, v)
        type(pt), intent(out) :: q
        integer, intent(in) :: v
        q%x = v
        q%y = -v
    end subroutine
end module

program gpu_derived_component_05
    use gpu_derived_component_05_m
    implicit none
    integer, parameter :: n = 100000
    type(pt) :: q, r
    type(u) :: y
    integer, allocatable :: a(:)
    integer :: i
    allocate(a(n), y%w(n))
    q = pt(0, 0)
    do concurrent (i = 1:n)
        a(i) = i
        if (i == 7) then
            q%x = 1
            call setp(q, 9)
        end if
    end do
    print *, q%x, q%y
    if (q%x /= 9 .or. q%y /= -9) error stop 1
    r = pt(0, 0)
    do concurrent (i = 1:n)
        call setp(r, i)
        a(i) = r%x + 2 * r%y
    end do
    do i = 1, n
        if (a(i) /= -i) error stop 2
    end do
    y%w = 0
    y%k = 0
    do concurrent (i = 1:n)
        y%w(i) = 2 * i
        if (i == 7) then
            y%k = 7
            call bump(y%k)
        end if
    end do
    print *, y%w(7), y%k
    do i = 1, n
        if (y%w(i) /= 2 * i) error stop 3
    end do
    if (y%k /= 8) error stop 4
    do concurrent (i = 1:n)
        y%k = i
        call bump(y%k)
        a(i) = y%k
    end do
    do i = 1, n
        if (a(i) /= i + 1) error stop 5
    end do
    r = pt(1, 1)
    y%k = 0
    print *, r%x, y%k
end program

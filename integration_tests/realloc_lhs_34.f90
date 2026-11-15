! Reallocation on assignment of an allocatable array that is a whole
! operand of its own elemental value, which keeps its shape and bounds,
! and of values that only reference it in another way, which can change it.
module realloc_lhs_34_mod
    implicit none
    type :: state_t
        real, allocatable :: x(:), v(:)
    end type
contains
    elemental real function twice(a)
        real, intent(in) :: a
        twice = 2*a
    end function

    elemental real function add(a, b)
        real, intent(in) :: a, b
        add = a + b
    end function

    function grow(a) result(r)
        real, intent(in) :: a(:)
        real :: r(size(a) + 1)
        r(:size(a)) = a
        r(size(a) + 1) = -1
    end function
end module

program realloc_lhs_34
    use realloc_lhs_34_mod
    implicit none
    real, allocatable :: x(:), y(:), m(:,:)
    integer, allocatable :: k(:)
    logical, allocatable :: mask(:)
    type(state_t) :: s
    type(state_t) :: arr(2)
    real :: dt
    integer :: i

    dt = 0.5
    allocate(x(3:5), y(3))
    x = [1., 2., 3.]
    y = [10., 20., 30.]

    ! The target is a whole operand of the elemental value
    x = x + dt*y
    call check(x, [6., 12., 18.], 3, 1)
    x = 2*x
    call check(x, [12., 24., 36.], 3, 2)
    x = -x
    call check(x, [-12., -24., -36.], 3, 3)
    x = abs(x)
    call check(x, [12., 24., 36.], 3, 4)
    x = merge(x, y, x > 20)
    call check(x, [10., 24., 36.], 3, 5)
    x = twice(x)
    call check(x, [20., 48., 72.], 3, 6)
    x = add(y, x)
    call check(x, [30., 68., 102.], 3, 7)
    x = x
    call check(x, [30., 68., 102.], 3, 8)
    x = real(int(x))
    call check(x, [30., 68., 102.], 3, 9)

    ! Masks and integers
    mask = [.true., .false., .true.]
    mask = .not. mask
    if (any(mask .neqv. [.false., .true., .false.])) error stop 10
    k = [1, 2, 3]
    k = k*k + 1
    if (lbound(k, 1) /= 1 .or. any(k /= [2, 5, 10])) error stop 11

    ! Components of a derived type
    s%x = [1., 2.]
    s%v = [3., 4.]
    s%x = s%x + dt*s%v
    if (any(s%x /= [2.5, 4.])) error stop 12
    arr(2)%x = [1., 2., 3., 4.]
    i = 2
    arr(i)%x = arr(i)%x * 3
    if (size(arr(2)%x) /= 4 .or. any(arr(2)%x /= [3., 6., 9., 12.])) error stop 13
    if (allocated(arr(1)%x)) error stop 14

    ! Rank 2
    allocate(m(0:1, 2:4))
    m = 1
    m = m + 1
    if (any(lbound(m) /= [0, 2]) .or. any(ubound(m) /= [1, 4])) error stop 15
    if (any(m /= 2)) error stop 16

    ! A scalar value does not reallocate
    x = 7
    call check(x, [7., 7., 7.], 3, 17)

    ! The target is referenced in the value, but not as a whole operand
    ! of an elemental operation: its shape can change
    x = [x, 1.]
    call check(x, [7., 7., 7., 1.], 1, 18)
    x = x(2:)
    call check(x, [7., 7., 1.], 1, 19)
    x = x(3:1:-1) + 1
    call check(x, [2., 8., 8.], 1, 20)
    x = grow(x)
    call check(x, [2., 8., 8., -1.], 1, 21)
    x = pack(x, x > 0)
    call check(x, [2., 8., 8.], 1, 22)
    x = y(1:2) + lbound(x, 1)
    call check(x, [11., 21.], 1, 23)
    x = [x, x] + sum(x)
    call check(x, [43., 53., 43., 53.], 1, 24)
    x = spread(x(1), 1, 2) + 1
    call check(x, [44., 44.], 1, 25)

    ! Another array of another shape
    y = x + 1
    call check(y, [45., 45.], 1, 26)

    print *, "ok"

contains

    subroutine check(a, expected, lb, code)
        real, allocatable, intent(in) :: a(:)
        real, intent(in) :: expected(:)
        integer, intent(in) :: lb, code
        if (.not. allocated(a)) error stop code
        if (size(a) /= size(expected)) error stop code
        if (any(a /= expected)) error stop code
        if (lbound(a, 1) /= lb) error stop code
        if (ubound(a, 1) /= lb + size(expected) - 1) error stop code
    end subroutine
end program

module array_section_assign_18_m
    ! With reallocation on assignment, `x(:) = value` still assigns to the
    ! allocated `x` as it is, without reallocating it, and as `x = value`
    ! does without reallocation: no temporary for the value, the result of
    ! a function computed into `x`.
    implicit none
    type :: particle
        real, allocatable :: r(:), d(:)
    end type
contains
    pure function twice(a) result(r)
        real, intent(in) :: a(:)
        real :: r(size(a))
        r = 2*a
    end function

    subroutine kernels(x, y, p, c2, a2, b2, e)
        real, allocatable, intent(inout) :: x(:), c2(:,:)
        real, intent(in) :: y(:), a2(:,:), b2(:,:)
        type(particle), intent(inout) :: p(:)
        real, intent(inout) :: e(:)
        x(:) = 0.5*x + y
        p(1)%d(:) = p(2)%r - p(1)%r
        c2(:,:) = matmul(a2, b2)
        e(:) = twice(y)
        x(:) = cshift(x, 1)
    end subroutine
end module

program array_section_assign_18
    use array_section_assign_18_m
    implicit none
    real, allocatable, target :: x(:), c2(:,:)
    real, pointer :: px(:), pc(:,:)
    real :: y(3), a2(2,2), b2(2,2), e(3)
    type(particle) :: p(2)
    integer :: i

    allocate(x(0:2), c2(2,2))
    x = [2., 4., 6.]
    y = [1., 2., 3.]
    a2 = 1
    b2 = 2
    e = 0
    px => x
    pc => c2
    do i = 1, 2
        allocate(p(i)%r(3), p(i)%d(3))
        p(i)%r = i
        p(i)%d = 0
    end do
    call kernels(x, y, p, c2, a2, b2, e)
    if (lbound(x, 1) /= 0 .or. ubound(x, 1) /= 2) error stop 1
    if (.not. associated(px, x)) error stop 2
    if (.not. associated(pc, c2)) error stop 3
    if (any(x /= [4., 6., 2.])) error stop 4
    if (any(p(1)%d /= 1)) error stop 5
    if (any(c2 /= 4)) error stop 6
    if (any(e /= [2., 4., 6.])) error stop 7

    ! An array constructor too is stored into the allocated target
    x(:) = [1., 2., 3.]
    if (.not. associated(px, x) .or. size(x) /= 3) error stop 8
    print *, x, e
end program

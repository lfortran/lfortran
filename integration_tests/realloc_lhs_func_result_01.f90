! Reallocation on assignment of the result of a function whose result
! shape is known before the reference (an explicit-shape result) to an
! allocatable variable: the variable takes the shape of the result, keeps
! its bounds if it already has that shape, and the reference sees the
! variable as it was before the assignment, whatever it is passed or can
! access.
module realloc_lhs_func_result_01_m
    implicit none
    real, allocatable :: g(:)

contains

    function twice(a) result(r)
        real, intent(in) :: a(:)
        real :: r(size(a))
        r = 2*a
    end function

    function twice2(a) result(r)
        real, intent(in) :: a(:,:)
        real :: r(size(a, 1), size(a, 2))
        r = 2*a
    end function

    function from_zero(n) result(r)
        integer, intent(in) :: n
        real :: r(0:n-1)
        integer :: i
        do i = 0, n - 1
            r(i) = i
        end do
    end function

    function reversed(a) result(r)
        real, intent(in) :: a(:)
        real :: r(size(a))
        integer :: i
        do i = 1, size(a)
            r(i) = a(size(a) + 1 - i)
        end do
    end function

    ! Reads the variable `g` of the module
    function plus_g(a) result(r)
        real, intent(in) :: a(:)
        real :: r(size(a))
        r = a + g(1)
    end function

    ! Reads the scalar `x` after the result is defined
    function scaled(a, x) result(r)
        real, intent(in) :: a(:)
        real, intent(in) :: x
        real :: r(size(a))
        r = 0
        r = a*x
    end function

    subroutine dummy_target(c, a)
        real, allocatable, intent(inout) :: c(:)
        real, intent(in) :: a(:)
        c = twice(a)
    end subroutine

    subroutine assumed_shape_target(c, a)
        real, intent(inout) :: c(:)
        real, intent(in) :: a(:)
        c = twice(a)
    end subroutine

    ! The internal function reads the variable of its host
    subroutine host_target(n)
        integer, intent(in) :: n
        real, allocatable :: h(:)
        allocate(h(n))
        h = 1
        h = plus_h([10., 20., 30.])
        if (size(h) /= 3) error stop "host size"
        if (any(h /= [11., 21., 31.])) error stop "host values"
    contains
        function plus_h(a) result(r)
            real, intent(in) :: a(:)
            real :: r(size(a))
            r = a + h(1)
        end function
    end subroutine

end module

program realloc_lhs_func_result_01
    use realloc_lhs_func_result_01_m
    implicit none
    real, allocatable :: c(:), m(:,:)
    real, allocatable, target :: t(:)
    real, pointer :: p(:)
    real :: a(3), e(5), x(4,3)
    real, target :: w(3)
    integer :: i

    a = [1., 2., 3.]

    ! Unallocated
    c = twice(a)
    if (.not. allocated(c)) error stop "unallocated"
    if (lbound(c, 1) /= 1 .or. size(c) /= 3) error stop "unallocated bounds"
    if (any(c /= [2., 4., 6.])) error stop "unallocated values"

    ! Allocated with the shape of the result: bounds kept
    deallocate(c)
    allocate(c(0:2))
    c = twice(a)
    if (lbound(c, 1) /= 0 .or. ubound(c, 1) /= 2) error stop "conforming bounds"
    if (any(c /= [2., 4., 6.])) error stop "conforming values"

    ! Allocated with another shape
    deallocate(c)
    allocate(c(-1:5))
    c = twice(a)
    if (lbound(c, 1) /= 1 .or. size(c) /= 3) error stop "nonconforming bounds"
    if (any(c /= [2., 4., 6.])) error stop "nonconforming values"

    ! The lower bound of a function reference is 1
    c = from_zero(4)
    if (lbound(c, 1) /= 1 .or. size(c) /= 4) error stop "from_zero bounds"
    if (any(c /= [0., 1., 2., 3.])) error stop "from_zero values"

    ! An allocatable dummy argument
    deallocate(c)
    call dummy_target(c, a)
    if (size(c) /= 3 .or. any(c /= [2., 4., 6.])) error stop "dummy"
    call dummy_target(c, [1., 1., 1., 1., 1.])
    if (size(c) /= 5 .or. any(c /= 2.)) error stop "dummy nonconforming"

    ! The variable is an actual argument
    c = [1., 2., 3., 4.]
    c = reversed(c)
    if (size(c) /= 4 .or. any(c /= [4., 3., 2., 1.])) error stop "c = reversed(c)"
    c = reversed(c(2:))
    if (size(c) /= 3 .or. any(c /= [1., 2., 3.])) error stop "c = reversed(c(2:))"
    c = scaled(a, c(2))
    if (size(c) /= 3 .or. any(c /= [2., 4., 6.])) error stop "c = scaled(a, c(2))"
    c = [1., 2., 3., 4.]
    c = scaled(a, real(size(c)))
    if (size(c) /= 3 .or. any(c /= [4., 8., 12.])) error stop "c = scaled(a, size(c))"
    c = [1., 2., 3., 4.]
    c = scaled(a, c(4))
    if (size(c) /= 3 .or. any(c /= [4., 8., 12.])) error stop "c = scaled(a, c(4))"

    ! The function reads the module variable
    g = [100., 200.]
    g = plus_g(a)
    if (size(g) /= 3 .or. any(g /= [101., 102., 103.])) error stop "module variable"

    ! The internal function reads the host variable
    call host_target(5)

    ! A target keeps its storage
    allocate(t(3))
    p => t
    t = twice(a)
    if (.not. associated(p, t)) error stop "target storage"
    if (any(p /= [2., 4., 6.])) error stop "target values"

    ! Rank 2
    do i = 1, 3
        x(:, i) = [1., 2., 3., 4.] + 10*i
    end do
    m = twice2(x)
    if (any(shape(m) /= [4, 3]) .or. any(m /= 2*x)) error stop "rank 2 unallocated"
    deallocate(m)
    allocate(m(0:3, 2:4))
    m = twice2(x)
    if (any(lbound(m) /= [0, 2])) error stop "rank 2 conforming bounds"
    if (any(m /= 2*x)) error stop "rank 2 conforming values"
    m = twice2(x(1:2, :))
    if (any(lbound(m) /= 1) .or. any(shape(m) /= [2, 3])) error stop "rank 2 nonconforming"
    if (any(m /= 2*x(1:2, :))) error stop "rank 2 nonconforming values"

    ! Targets that are not allocatable
    e = 0
    e(2:4) = twice(a)
    if (any(e /= [0., 2., 4., 6., 0.])) error stop "section"
    call assumed_shape_target(e(1:3), a)
    if (any(e /= [2., 4., 6., 6., 0.])) error stop "assumed shape"
    p => w
    p = twice(a)
    if (any(w /= [2., 4., 6.])) error stop "pointer"
    print *, c, g, m
end program

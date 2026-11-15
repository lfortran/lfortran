! Reallocation on assignment of the allocatable array result of a function
! to an allocatable variable: the variable takes the shape of the result
! with lower bounds of 1, unless it already has that shape, when it keeps
! its bounds and its storage.
module realloc_lhs_func_result_03_m
    implicit none
    real, allocatable :: g(:)

    type :: holder
        real, allocatable :: x(:)
    end type

    type :: item
        integer :: i = -1
    end type

contains

    function twice(a) result(r)
        real, intent(in) :: a(:)
        real, allocatable :: r(:)
        allocate(r(0:size(a)-1))
        r = 2*a
    end function

    function twice2(a) result(r)
        integer, intent(in) :: a(:,:)
        integer, allocatable :: r(:,:)
        allocate(r(-1:size(a, 1)-2, 3:size(a, 2)+2))
        r = 2*a
    end function

    function plus_g(a) result(r)
        real, intent(in) :: a(:)
        real, allocatable :: r(:)
        allocate(r(size(a)))
        r = a + g(1)
    end function

    function words(n) result(r)
        integer, intent(in) :: n
        character(len=:), allocatable :: r(:)
        integer :: i
        allocate(character(len=n) :: r(n))
        do i = 1, n
            r(i) = repeat(achar(iachar('a') + i - 1), n)
        end do
    end function

    function items(n) result(r)
        integer, intent(in) :: n
        type(item), allocatable :: r(:)
        integer :: i
        allocate(r(n))
        do i = 1, n
            r(i)%i = i
        end do
    end function

    function flags(n) result(r)
        integer, intent(in) :: n
        logical, allocatable :: r(:)
        allocate(r(n))
        r = .true.
        r(1) = .false.
    end function

    subroutine dummy_target(c, a)
        real, allocatable, intent(inout) :: c(:)
        real, intent(in) :: a(:)
        c = twice(a)
    end subroutine

end module

program realloc_lhs_func_result_03
    use realloc_lhs_func_result_03_m
    implicit none
    real, allocatable :: c(:)
    real, allocatable, target :: t(:)
    real, pointer :: p(:)
    integer, allocatable :: m(:,:)
    integer :: x(2,3)
    character(len=:), allocatable :: s(:)
    type(item), allocatable :: it(:)
    type(holder) :: h
    logical, allocatable :: l(:)

    ! Unallocated
    c = twice([1., 2., 3.])
    if (lbound(c, 1) /= 1 .or. size(c) /= 3) error stop "unallocated bounds"
    if (any(c /= [2., 4., 6.])) error stop "unallocated values"

    ! Allocated with the shape of the result, lower bound 1
    c = twice([3., 2., 1.])
    if (lbound(c, 1) /= 1 .or. size(c) /= 3) error stop "conforming bounds"
    if (any(c /= [6., 4., 2.])) error stop "conforming values"

    ! Allocated with the shape of the result, other bounds
    deallocate(c)
    allocate(c(5:7))
    c = twice([1., 2., 3.])
    if (lbound(c, 1) /= 5 .or. ubound(c, 1) /= 7) error stop "conforming other bounds"
    if (any(c /= [2., 4., 6.])) error stop "conforming other bounds values"

    ! Allocated with another shape
    c = twice([1., 2.])
    if (lbound(c, 1) /= 1 .or. size(c) /= 2) error stop "nonconforming bounds"
    if (any(c /= [2., 4.])) error stop "nonconforming values"

    ! The variable is the actual argument
    c = twice(c)
    if (lbound(c, 1) /= 1 .or. any(c /= [4., 8.])) error stop "c = twice(c)"
    c = [1., 2., 3., 4.]
    c = twice(c(2:3))
    if (size(c) /= 2 .or. any(c /= [4., 6.])) error stop "c = twice(c(2:3))"

    ! An allocatable dummy argument
    call dummy_target(c, [5., 6.])
    if (size(c) /= 2 .or. any(c /= [10., 12.])) error stop "dummy"

    ! A target keeps its storage
    allocate(t(3))
    p => t
    t = twice([1., 2., 3.])
    if (.not. associated(p, t)) error stop "target storage"
    if (any(p /= [2., 4., 6.])) error stop "target values"
    t = twice([1., 2.])
    if (size(t) /= 2 .or. lbound(t, 1) /= 1 .or. any(t /= [2., 4.])) error stop "target nonconforming"

    ! The function reads the module variable
    g = [10., 20.]
    g = plus_g([1., 2., 3.])
    if (size(g) /= 3 .or. any(g /= [11., 12., 13.])) error stop "module variable"

    ! A component
    h%x = twice([1., 2.])
    if (lbound(h%x, 1) /= 1 .or. any(h%x /= [2., 4.])) error stop "component"
    deallocate(h%x)
    allocate(h%x(0:1))
    h%x = twice([3., 4.])
    if (lbound(h%x, 1) /= 0 .or. any(h%x /= [6., 8.])) error stop "component conforming"

    ! Rank 2
    x = reshape([1, 2, 3, 4, 5, 6], [2, 3])
    m = twice2(x)
    if (any(lbound(m) /= 1) .or. any(shape(m) /= [2, 3])) error stop "rank 2 bounds"
    if (any(m /= 2*x)) error stop "rank 2 values"
    deallocate(m)
    allocate(m(0:1, -2:0))
    m = twice2(x)
    if (any(lbound(m) /= [0, -2])) error stop "rank 2 conforming bounds"
    if (any(m /= 2*x)) error stop "rank 2 conforming values"
    m = twice2(x(:, 1:2))
    if (any(lbound(m) /= 1) .or. any(shape(m) /= [2, 2])) error stop "rank 2 nonconforming"
    if (any(m /= 2*x(:, 1:2))) error stop "rank 2 nonconforming values"

    ! Character, derived and logical types
    s = words(2)
    if (len(s) /= 2 .or. size(s) /= 2 .or. s(2) /= "bb") error stop "character"
    s = words(3)
    if (len(s) /= 3 .or. size(s) /= 3 .or. s(3) /= "ccc") error stop "character other length"
    it = items(3)
    if (size(it) /= 3 .or. it(3)%i /= 3) error stop "derived type"
    l = flags(3)
    if (size(l) /= 3 .or. l(1) .or. .not. l(3)) error stop "logical"
    print *, c, t, g, h%x, m, s, it%i, l
end program

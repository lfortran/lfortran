module array_section_assign_16_m
    ! `x(:) = value` keeps the semantics of the assignment to a section:
    ! the value is evaluated before the target is defined, the target keeps
    ! its allocation and bounds, and is assigned through a pointer or a
    ! dummy argument wherever it is.
    implicit none
    type :: holder
        real, allocatable :: c(:)
        real :: f(3)
    end type
    type :: node
        type(holder) :: h
    end type
contains
    function reversed(a) result(r)
        real, intent(in) :: a(:)
        real :: r(size(a))
        integer :: k
        do k = 1, size(a)
            r(k) = a(size(a) + 1 - k)
        end do
    end function

    function counted(n) result(r)
        integer, intent(in) :: n
        real, allocatable :: r(:)
        integer :: k
        allocate(r(n))
        do k = 1, n
            r(k) = 10*k
        end do
    end function

    subroutine assumed(x, y)
        real, intent(inout) :: x(:)
        real, intent(in) :: y(:)
        x(:) = y(size(y):1:-1) + x
    end subroutine

    subroutine check(x, ref, code)
        real, intent(in) :: x(:), ref(:)
        integer, intent(in) :: code
        if (size(x) /= size(ref)) error stop code
        if (any(x /= ref)) error stop code
    end subroutine
end module

program array_section_assign_16
    use array_section_assign_16_m
    implicit none
    real, allocatable :: x(:), z(:), m2(:,:), m3(:,:,:)
    real, allocatable, target :: t(:)
    real, target :: big(8)
    real, pointer :: p(:)
    real :: e(4), mat(4,4)
    integer :: idx(4), i
    character(len=3) :: s(3)
    character(len=3), allocatable :: sa(:)
    type(holder) :: h, hs(2)
    type(node) :: nd(2)
    type(holder), allocatable :: ha(:), hb(:)

    ! Overlap: the value reads the target in another order
    allocate(x(4))
    x = [1., 2., 3., 4.]
    x(:) = x(4:1:-1)
    call check(x, [4., 3., 2., 1.], 1)
    x(:) = [x(2), x(1), x(4), x(3)]
    call check(x, [3., 4., 1., 2.], 2)
    idx = [4, 3, 2, 1]
    x(:) = x(idx)
    call check(x, [2., 1., 4., 3.], 3)
    x(:) = cshift(x, 1)
    call check(x, [1., 4., 3., 2.], 4)
    mat = 0
    do i = 1, 4
        mat(i, 5 - i) = 1
    end do
    x(:) = matmul(mat, x)
    call check(x, [2., 3., 4., 1.], 5)
    x(:) = reversed(x)
    call check(x, [1., 4., 3., 2.], 6)
    x(:) = 0.5*x + x(4:1:-1)
    call check(x, [2.5, 5., 5.5, 2.], 7)
    e = [1., 2., 3., 4.]
    e(:) = e(4:1:-1)
    call check(e, [4., 3., 2., 1.], 8)

    ! The lower bounds of the target stay, also for a function result
    deallocate(x)
    allocate(x(0:2))
    x(:) = [1., 2., 3.]
    if (lbound(x, 1) /= 0 .or. x(0) /= 1) error stop 9
    x(:) = counted(3)
    if (lbound(x, 1) /= 0 .or. ubound(x, 1) /= 2) error stop 10
    call check(x, [10., 20., 30.], 11)
    x(:) = reversed(x)
    if (lbound(x, 1) /= 0) error stop 12
    call check(x, [30., 20., 10.], 13)

    ! A pointer to a non-contiguous section, and a pointer into the target
    big = 0
    p => big(2::2)
    p(:) = [1., 2., 3., 4.]
    call check(big, [0., 1., 0., 2., 0., 3., 0., 4.], 14)
    p(:) = cshift(p, 1)
    call check(big, [0., 2., 0., 3., 0., 4., 0., 1.], 15)
    allocate(t(4))
    t = [1., 2., 3., 4.]
    p => t
    t(:) = cshift(p, -1)
    call check(t, [4., 1., 2., 3.], 16)
    if (.not. associated(p, t)) error stop 17

    ! An assumed-shape dummy argument
    e = [1., 2., 3., 4.]
    call assumed(e, [10., 20., 30., 40.])
    call check(e, [41., 32., 23., 14.], 18)

    ! Components, of a scalar and of array elements
    allocate(h%c(3))
    h%c(:) = [1., 2., 3.]
    h%f(:) = h%c(3:1:-1)
    call check(h%f, [3., 2., 1.], 19)
    do i = 1, 2
        allocate(hs(i)%c(3), nd(i)%h%c(3))
        hs(i)%c(:) = i
        nd(i)%h%c(:) = 10*i
    end do
    hs(1)%c(:) = hs(2)%c - hs(1)%c
    call check(hs(1)%c, [1., 1., 1.], 20)
    hs(2)%c(:) = hs(2)%c(3:1:-1) + [1., 2., 3.]
    call check(hs(2)%c, [3., 4., 5.], 21)
    nd(2)%h%c(:) = nd(1)%h%c + nd(2)%h%c
    call check(nd(2)%h%c, [30., 30., 30.], 22)
    hs(1)%f(:) = 7
    call check(hs(1)%f, [7., 7., 7.], 23)

    ! Derived-type elements with allocatable components are deep copies
    allocate(ha(2), hb(2))
    do i = 1, 2
        allocate(ha(i)%c(i), hb(i)%c(3))
        ha(i)%c = i
        hb(i)%c = 0
    end do
    hb(:) = ha
    ha(1)%c(1) = 5
    if (size(hb(1)%c) /= 1 .or. size(hb(2)%c) /= 2) error stop 24
    if (hb(1)%c(1) /= 1 .or. any(hb(2)%c /= 2)) error stop 25
    hb(:) = hb(2:1:-1)
    if (size(hb(1)%c) /= 2 .or. hb(2)%c(1) /= 1) error stop 26

    ! Character arrays
    s = ['abc', 'def', 'ghi']
    s(:) = s(3:1:-1)
    if (s(1) /= 'ghi' .or. s(3) /= 'abc') error stop 27
    s(:) = s(:)(2:3) // 'x'
    if (s(1) /= 'hix' .or. s(2) /= 'efx') error stop 28
    allocate(sa(2))
    sa(:) = ['xy', 'zw']
    if (sa(1) /= 'xy ' .or. sa(2) /= 'zw ') error stop 29

    ! Zero-size targets
    allocate(z(0))
    z(:) = 1
    z(:) = z(0:1:-1) + 1
    if (size(z) /= 0) error stop 30

    ! Rank 2 and 3, other lower bounds
    allocate(m2(0:1, 3), m3(2, 2, -1:0))
    m2(:, :) = reshape([1., 2., 3., 4., 5., 6.], [2, 3])
    m2(:, :) = m2(1:0:-1, :)
    if (m2(0, 1) /= 2 .or. m2(1, 3) /= 5) error stop 31
    if (lbound(m2, 1) /= 0) error stop 32
    m3(:, :, :) = 1
    m3(:, :, :) = m3 + spread(m2(:, 1:2), 3, 2)
    if (m3(1, 1, -1) /= 3 .or. m3(2, 2, 0) /= 4) error stop 33
    print *, x, e, m2
end program

! An elementwise assignment whose value reads the target only through the
! very same section (or whole array) reads every element at the position it
! assigns, before it assigns it, so no temporary is needed (issue #16105).
! Every result is checked against an element-by-element loop.
module array_section_assign_09_mod
    implicit none
    type :: dense_t
        real, allocatable :: dw(:,:)
        real, allocatable :: w(:,:)
        real :: lr
    end type
    type :: net_t
        type(dense_t), allocatable :: layers(:)
    end type
contains

    subroutine row_sec(a, x, i, m)
        real, intent(inout) :: a(:,:)
        real, intent(in) :: x(:)
        integer, intent(in) :: i, m
        a(i,1:m) = a(i,1:m) + x(1:m)*2.0
    end subroutine

    subroutine row_scale(a, i, m)
        real, intent(inout) :: a(:,:)
        integer, intent(in) :: i, m
        a(i,1:m) = 2.0*a(i,1:m)
    end subroutine

    subroutine row_neg(a, i, m)
        real, intent(inout) :: a(:,:)
        integer, intent(in) :: i, m
        a(i,1:m) = -a(i,1:m)
    end subroutine

    subroutine row_twice(a, x, i, m)
        real, intent(inout) :: a(:,:)
        real, intent(in) :: x(:)
        integer, intent(in) :: i, m
        a(i,1:m) = (a(i,1:m) + x(1:m))*(a(i,1:m) - x(1:m))
    end subroutine

    subroutine v_sec(v, x, m)
        real, intent(inout) :: v(:)
        real, intent(in) :: x(:)
        integer, intent(in) :: m
        v(1:m) = v(1:m) + x(1:m)
    end subroutine

    subroutine v_colon(v, x)
        real, intent(inout) :: v(:)
        real, intent(in) :: x(:)
        v(:) = v(:) + x(:)
    end subroutine

    subroutine col_sec(a, x, i, m)
        real, intent(inout) :: a(:,:)
        real, intent(in) :: x(:)
        integer, intent(in) :: i, m
        a(1:m,i) = a(1:m,i) + x(1:m)
    end subroutine

    subroutine col(a, x, i)
        real, intent(inout) :: a(:,:)
        real, intent(in) :: x(:)
        integer, intent(in) :: i
        a(:,i) = a(:,i) + x(:)
    end subroutine

    subroutine row(a, x, i)
        real, intent(inout) :: a(:,:)
        real, intent(in) :: x(:)
        integer, intent(in) :: i
        a(i,:) = a(i,:) + x(:)
    end subroutine

    subroutine whole(a)
        real, intent(inout) :: a(:,:)
        a = a + 1.0
    end subroutine

    subroutine whole_explicit(a, n)
        integer, intent(in) :: n
        real, intent(inout) :: a(n,n)
        a = a*2.0 - 1.0
    end subroutine

    subroutine row_explicit(a, x, n, i)
        integer, intent(in) :: n, i
        real, intent(inout) :: a(n,n)
        real, intent(in) :: x(n)
        a(i,:) = a(i,:) + x
    end subroutine

    subroutine local_array(x, r)
        real, intent(in) :: x(8)
        real, intent(out) :: r
        real :: a(8,8)
        integer :: i, j
        do j = 1, 8
            do i = 1, 8
                a(i,j) = real(i + 10*j)
            end do
        end do
        a = a + 1.0
        a(3,:) = a(3,:) + x
        r = sum(a)
    end subroutine

    subroutine int_sec(ia, m)
        integer, intent(inout) :: ia(:)
        integer, intent(in) :: m
        ia(1:m) = ia(1:m)*2 + 1
    end subroutine

    subroutine logical_sec(l, mk)
        logical, intent(inout) :: l(:)
        logical, intent(in) :: mk(:)
        l(:) = l(:) .and. mk(:)
    end subroutine

    subroutine logical_cmp(l, v)
        logical, intent(inout) :: l(:)
        real, intent(in) :: v(:)
        l(:) = l(:) .neqv. (v(:) > 2.0)
    end subroutine

    subroutine complex_sec(c, m)
        complex, intent(inout) :: c(:)
        integer, intent(in) :: m
        c(1:m) = c(1:m)*(0.0, 1.0)
    end subroutine

    subroutine strided(v, n)
        real, intent(inout) :: v(:)
        integer, intent(in) :: n
        v(1:n:2) = v(1:n:2) + 1.0
    end subroutine

    subroutine reversed(v, x, n)
        real, intent(inout) :: v(:)
        real, intent(in) :: x(:)
        integer, intent(in) :: n
        v(n:1:-1) = v(n:1:-1) + x(1:n)
    end subroutine

    subroutine dense_do(self, input, db)
        type(dense_t), intent(inout) :: self
        real, intent(in) :: input(:), db(:)
        integer :: i
        do i = 1, size(db)
            self%dw(:,i) = self%dw(:,i) + input(:)*db(i)
        end do
    end subroutine

    subroutine dense_dc(self, input, db)
        type(dense_t), intent(inout) :: self
        real, intent(in) :: input(:), db(:)
        integer :: i
        do concurrent (i = 1:size(db))
            self%dw(:,i) = self%dw(:,i) + input(:)*db(i)
        end do
    end subroutine

    subroutine dense_class(self, input, db)
        class(dense_t), intent(inout) :: self
        real, intent(in) :: input(:), db(:)
        integer :: i
        do concurrent (i = 1:size(db))
            self%dw(:,i) = self%dw(:,i) - input(:)*db(i)
        end do
    end subroutine

    subroutine dense_lr(self, i)
        type(dense_t), intent(inout) :: self
        integer, intent(in) :: i
        self%dw(:,i) = self%dw(:,i)*self%lr + self%w(:,i)
    end subroutine

    subroutine net_chain(net, k, i, x)
        type(net_t), intent(inout) :: net
        integer, intent(in) :: k, i
        real, intent(in) :: x(:)
        net%layers(k)%w(:,i) = net%layers(k)%w(:,i) + x(:)
    end subroutine

end module

program array_section_assign_09
    use array_section_assign_09_mod
    implicit none
    integer, parameter :: n = 6
    real :: a(n,n), a0(n,n), x(n), v(n), v0(n), r, r0
    integer :: ia(n), ia0(n), i, j
    logical :: l(n), l0(n), mk(n)
    complex :: c(n), c0(n)
    type(dense_t) :: d
    type(net_t) :: net
    real :: db(3)

    do i = 1, n
        x(i) = real(i)
        do j = 1, n
            a(i,j) = real(i + 10*j)
        end do
    end do

    a0 = a
    call row_sec(a, x, 2, 5)
    do j = 1, 5
        a0(2,j) = a0(2,j) + x(j)*2.0
    end do
    if (any(a /= a0)) error stop 1

    call row_scale(a, 3, 4)
    do j = 1, 4
        a0(3,j) = 2.0*a0(3,j)
    end do
    if (any(a /= a0)) error stop 2

    call row_neg(a, 4, n)
    do j = 1, n
        a0(4,j) = -a0(4,j)
    end do
    if (any(a /= a0)) error stop 3

    call row_twice(a, x, 5, 3)
    do j = 1, 3
        a0(5,j) = (a0(5,j) + x(j))*(a0(5,j) - x(j))
    end do
    if (any(a /= a0)) error stop 4

    call col_sec(a, x, 2, 4)
    do i = 1, 4
        a0(i,2) = a0(i,2) + x(i)
    end do
    if (any(a /= a0)) error stop 5

    call col(a, x, 6)
    do i = 1, n
        a0(i,6) = a0(i,6) + x(i)
    end do
    if (any(a /= a0)) error stop 6

    call row(a, x, 1)
    do j = 1, n
        a0(1,j) = a0(1,j) + x(j)
    end do
    if (any(a /= a0)) error stop 7

    call whole(a)
    do j = 1, n
        do i = 1, n
            a0(i,j) = a0(i,j) + 1.0
        end do
    end do
    if (any(a /= a0)) error stop 8

    call whole_explicit(a, n)
    do j = 1, n
        do i = 1, n
            a0(i,j) = a0(i,j)*2.0 - 1.0
        end do
    end do
    if (any(a /= a0)) error stop 9

    call row_explicit(a, x, n, 6)
    do j = 1, n
        a0(6,j) = a0(6,j) + x(j)
    end do
    if (any(a /= a0)) error stop 10

    call local_array([(real(i), i = 1, 8)], r)
    r0 = 0.0
    do j = 1, 8
        do i = 1, 8
            r0 = r0 + real(i + 10*j) + 1.0
        end do
        r0 = r0 + real(j)
    end do
    if (r /= r0) error stop 11

    v = x
    v0 = v
    call v_sec(v, x, 4)
    do i = 1, 4
        v0(i) = v0(i) + x(i)
    end do
    if (any(v /= v0)) error stop 12

    call v_colon(v, x)
    do i = 1, n
        v0(i) = v0(i) + x(i)
    end do
    if (any(v /= v0)) error stop 13

    call strided(v, n)
    do i = 1, n, 2
        v0(i) = v0(i) + 1.0
    end do
    if (any(v /= v0)) error stop 14

    call reversed(v, x, 5)
    do i = 1, 5
        v0(6 - i) = v0(6 - i) + x(i)
    end do
    if (any(v /= v0)) error stop 15

    ia = [(i, i = 1, n)]
    ia0 = ia
    call int_sec(ia, 5)
    do i = 1, 5
        ia0(i) = ia0(i)*2 + 1
    end do
    if (any(ia /= ia0)) error stop 16

    l = [.true., .true., .false., .true., .false., .true.]
    mk = [.true., .false., .true., .true., .false., .false.]
    l0 = l
    call logical_sec(l, mk)
    do i = 1, n
        l0(i) = l0(i) .and. mk(i)
    end do
    if (any(l .neqv. l0)) error stop 17

    call logical_cmp(l, x)
    do i = 1, n
        l0(i) = l0(i) .neqv. (x(i) > 2.0)
    end do
    if (any(l .neqv. l0)) error stop 18

    do i = 1, n
        c(i) = cmplx(real(i), real(-i))
    end do
    c0 = c
    call complex_sec(c, 4)
    do i = 1, 4
        c0(i) = c0(i)*(0.0, 1.0)
    end do
    if (any(c /= c0)) error stop 19

    db = [1.0, 2.0, 3.0]
    allocate(d%dw(n,3), d%w(n,3))
    d%dw = 1.0
    d%w = 2.0
    d%lr = 0.5
    call dense_do(d, x, db)
    call dense_dc(d, x, db)
    do j = 1, 3
        do i = 1, n
            if (d%dw(i,j) /= 1.0 + 2.0*x(i)*db(j)) error stop 20
        end do
    end do
    call dense_class(d, x, db)
    do j = 1, 3
        do i = 1, n
            if (d%dw(i,j) /= 1.0 + x(i)*db(j)) error stop 26
        end do
    end do
    call dense_dc(d, x, db)
    call dense_lr(d, 2)
    do i = 1, n
        if (d%dw(i,2) /= (1.0 + 2.0*x(i)*db(2))*0.5 + 2.0) error stop 21
        if (d%dw(i,1) /= 1.0 + 2.0*x(i)*db(1)) error stop 22
    end do

    allocate(net%layers(2))
    allocate(net%layers(1)%w(n,2), net%layers(2)%w(n,2))
    net%layers(1)%w = 1.0
    net%layers(2)%w = 3.0
    call net_chain(net, 2, 1, x)
    do i = 1, n
        if (net%layers(2)%w(i,1) /= 3.0 + x(i)) error stop 23
        if (net%layers(2)%w(i,2) /= 3.0) error stop 24
        if (net%layers(1)%w(i,1) /= 1.0) error stop 25
    end do

    print *, sum(a), sum(v), sum(ia), sum(d%dw)
end program

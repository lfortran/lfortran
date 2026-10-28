! Assignments whose value may read elements of the target other than the
! one being assigned must still be evaluated completely before the target
! is defined (issue #16105): shifted, reversed, differently subscripted or
! vector-subscripted references, elements, reductions and other
! non-elementwise intrinsics of the target, array constructors, function
! references, pointers, associate names, EQUIVALENCE, COMMON and WHERE.
module array_section_assign_10_c1
    implicit none
    real :: ca(4)
    common /asa10/ ca
end module

module array_section_assign_10_c2
    implicit none
    ! cc(1:3) shares the storage of ca(2:4)
    real :: cw, cc(3)
    common /asa10/ cw, cc
end module

module array_section_assign_10_eq
    implicit none
    ! nq shares the storage of iq(3)
    integer :: iq(6), nq
    equivalence (nq, iq(3))
end module

module array_section_assign_10_mod
    implicit none
    real :: vm(5)
    type :: dense_t
        real, allocatable :: dw(:,:)
    end type
contains

    function f() result(r)
        real :: r(5)
        r = vm(5:1:-1)
    end function

    elemental real function g(x)
        real, intent(in) :: x
        g = x + vm(1)
    end function

    pure integer function k3()
        k3 = 3
    end function

    subroutine shift_row(a, x, i, m)
        real, intent(inout) :: a(:,:)
        real, intent(in) :: x(:)
        integer, intent(in) :: i, m
        a(i,2:m) = a(i,1:m-1) + x(1:m-1)
    end subroutine

    subroutine same_and_reversed(a, i, m)
        real, intent(inout) :: a(:,:)
        integer, intent(in) :: i, m
        a(i,1:m) = a(i,1:m) + a(i,m:1:-1)
    end subroutine

    subroutine other_row(a, i, j)
        real, intent(inout) :: a(:,:)
        integer, intent(in) :: i, j
        a(i,:) = a(j,:) + a(i,:)
    end subroutine

    subroutine component_sections(s)
        type(dense_t), intent(inout) :: s
        s%dw(:,1) = s%dw(:,1) + s%dw(:,2)
        s%dw(:,2) = s%dw(:,2) + s%dw(1,2)
    end subroutine

end module

program array_section_assign_10
    use array_section_assign_10_mod
    use array_section_assign_10_c1
    use array_section_assign_10_c2
    use array_section_assign_10_eq
    implicit none
    integer, parameter :: n = 5
    real :: a(n,n), a0(n,n), v(n), v0(n), x(n), mat(n,n), w(n)
    real, target :: t(n)
    real, pointer :: p(:)
    real :: e1(6), e2(6), f1(6), f2(5)
    equivalence (e1, e2)
    equivalence (f1(2), f2(1))
    integer :: idx(3), i, j, k
    logical :: msk(n)
    type(dense_t) :: s

    do i = 1, n
        x(i) = real(i)
        do j = 1, n
            a(i,j) = real(i + 10*j)
            mat(i,j) = real(i - j)
        end do
    end do

    ! shifted sections
    v = x
    v(2:n) = v(1:n-1) + 0.0
    if (any(v /= [1.0, 1.0, 2.0, 3.0, 4.0])) error stop 1
    v = x
    v(1:n-1) = v(2:n) + 0.0
    if (any(v /= [2.0, 3.0, 4.0, 5.0, 5.0])) error stop 2
    a0 = a
    call shift_row(a, x, 2, n)
    do j = 2, n
        a0(2,j) = real(2 + 10*(j-1)) + x(j-1)
    end do
    if (any(a /= a0)) error stop 3

    ! reversed against forward
    v = x
    v(1:n) = v(n:1:-1) + v(1:n)
    if (any(v /= 6.0)) error stop 4
    a0 = a
    call same_and_reversed(a, 3, n)
    a0(3,:) = [(real(3 + 10*j) + real(3 + 10*(n+1-j)), j = 1, n)]
    if (any(a /= a0)) error stop 5

    ! other row, with i == j and i /= j
    a0 = a
    call other_row(a, 1, 1)
    a0(1,:) = 2.0*a0(1,:)
    if (any(a /= a0)) error stop 6
    a0 = a
    call other_row(a, 4, 5)
    do j = 1, n
        a0(4,j) = a0(5,j) + a0(4,j)
    end do
    if (any(a /= a0)) error stop 7

    ! vector subscripts
    idx = [3, 1, 2]
    v = x
    v(idx) = v(idx) + 1.0
    if (any(v /= [2.0, 3.0, 4.0, 4.0, 5.0])) error stop 8
    v = x
    v(1:3) = v(1:3) + v(idx)
    if (any(v /= [4.0, 3.0, 5.0, 4.0, 5.0])) error stop 9

    ! an element of the target
    v = x
    v = v + v(1)
    if (any(v /= [2.0, 3.0, 4.0, 5.0, 6.0])) error stop 10
    v = x
    v(2:n) = v(2:n) + v(2)
    if (any(v /= [1.0, 4.0, 5.0, 6.0, 7.0])) error stop 11
    allocate(s%dw(n,2))
    s%dw(:,1) = x
    s%dw(:,2) = 2.0*x
    call component_sections(s)
    if (any(s%dw(:,1) /= 3.0*x)) error stop 12
    if (any(s%dw(:,2) /= 2.0*x + 2.0)) error stop 13

    ! reductions
    v = x
    v = v/maxval(v)
    if (any(v /= x/5.0)) error stop 14
    v = x
    v = v + dot_product(v, v)
    if (any(v /= x + 55.0)) error stop 15
    a0 = a
    a(:,1) = a(:,1) + sum(a, dim=2)
    do i = 1, n
        a0(i,1) = a0(i,1) + sum(a0(i,:))
    end do
    if (any(a /= a0)) error stop 16

    ! non-elementwise intrinsics
    v = x
    v0 = v + matmul(mat, x)
    v = v + matmul(mat, v)
    if (any(v /= v0)) error stop 17
    a0 = a
    a = a + transpose(a)
    do j = 1, n
        do i = 1, n
            if (a(i,j) /= a0(i,j) + a0(j,i)) error stop 18
        end do
    end do
    a0 = a
    a = a + reshape(a, [n, n], order=[2, 1])
    do j = 1, n
        do i = 1, n
            if (a(i,j) /= a0(i,j) + a0(j,i)) error stop 19
        end do
    end do
    a0 = a
    a = a + spread(a(:,1), 2, n)
    do j = 1, n
        do i = 1, n
            if (a(i,j) /= a0(i,j) + a0(i,1)) error stop 20
        end do
    end do
    v = x
    v = v + cshift(v, 1)
    if (any(v /= [3.0, 5.0, 7.0, 9.0, 6.0])) error stop 21
    v = x
    v = v + eoshift(v, 1)
    if (any(v /= [3.0, 5.0, 7.0, 9.0, 5.0])) error stop 22
    v = [1.0, -2.0, 3.0, -4.0, 5.0]
    k = count(v > 0.0)
    v(1:k) = v(1:k) + pack(v, v > 0.0)
    if (any(v /= [2.0, 1.0, 8.0, -4.0, 5.0])) error stop 23

    ! array constructor
    v = x
    v(1:3) = v(1:3) + [v(3), v(2), v(1)]
    if (any(v /= [4.0, 4.0, 4.0, 4.0, 5.0])) error stop 24

    ! function references reading the target
    vm = x
    vm = vm + f()
    if (any(vm /= 6.0)) error stop 25
    vm = x
    vm = vm + g(vm)
    if (any(vm /= 2.0*x + 1.0)) error stop 26
    v = x
    v(1:k3()) = v(1:k3()) + 1.0
    if (any(v /= [2.0, 3.0, 4.0, 4.0, 5.0])) error stop 27

    ! a pointer to the target
    t = x
    p => t(n:1:-1)
    t = t + p
    if (any(t /= 6.0)) error stop 28
    t = x
    p => t
    w = t
    p(1:n) = p(1:n) + w(n:1:-1)
    if (any(t /= 6.0)) error stop 29

    ! an associate name of the target
    v = x
    associate (b => v(2:n))
        v(1:n-1) = v(1:n-1) + b
    end associate
    if (any(v /= [3.0, 5.0, 7.0, 9.0, 5.0])) error stop 30
    v = x
    associate (b => v)
        b(:) = b(:) + v(:)
    end associate
    if (any(v /= 2.0*x)) error stop 31

    ! EQUIVALENCE
    e1 = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
    e1(1:5) = e1(1:5) + e2(2:6)
    if (any(e1 /= [3.0, 5.0, 7.0, 9.0, 11.0, 6.0])) error stop 32
    e1 = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
    e2(1:5) = e2(1:5) + e1(2:6)
    if (any(e1 /= [3.0, 5.0, 7.0, 9.0, 11.0, 6.0])) error stop 34
    e1 = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
    e1 = e1 + e2
    if (any(e1 /= 2.0*[1.0, 2.0, 3.0, 4.0, 5.0, 6.0])) error stop 35
    ! f1(2:6) and f2 share their storage
    f1 = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
    f1(2:6) = f1(2:6) + f2(1:5)
    if (any(f1 /= [1.0, 4.0, 6.0, 8.0, 10.0, 12.0])) error stop 36
    f1 = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
    f1(1:5) = f1(1:5) + f2(1:5)
    if (any(f1 /= [3.0, 5.0, 7.0, 9.0, 11.0, 6.0])) error stop 37
    f1 = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
    f2 = f2 + f1(2:6)
    if (any(f1 /= [1.0, 4.0, 6.0, 8.0, 10.0, 12.0])) error stop 38
    ! EQUIVALENCE in a module: a subscript shares the storage of the target
    iq = [1, 2, 3, 4, 5, 6]
    iq(1:nq) = iq(1:nq) + 10
    if (any(iq /= [11, 12, 13, 4, 5, 6])) error stop 39
    iq = [1, 2, 3, 4, 5, 6]
    iq(nq:6) = iq(nq:6) * 2
    if (any(iq /= [1, 2, 6, 8, 10, 12])) error stop 40

    ! COMMON views with different layouts in two modules
    ca = [1.0, 10.0, 100.0, 1000.0]
    ca(2:4) = ca(2:4) + cc(1:3)
    if (any(ca /= [1.0, 20.0, 200.0, 2000.0])) error stop 41
    ca = [1.0, 10.0, 100.0, 1000.0]
    cc(1:2) = cc(1:2) + ca(3:4)
    if (any(ca /= [1.0, 110.0, 1100.0, 1000.0])) error stop 42
    ca = [1.0, 10.0, 100.0, 1000.0]
    call common_shift()
    if (any(ca /= [1.0, 11.0, 110.0, 1100.0])) error stop 43

    ! WHERE
    v = x
    msk = [.true., .true., .false., .true., .true.]
    where (msk) v = v + v(n:1:-1)
    if (any(v /= [6.0, 6.0, 3.0, 6.0, 6.0])) error stop 33

    print *, sum(a), sum(v), sum(t), sum(e1), sum(f1), sum(ca)
end program

! The same COMMON block as array_section_assign_10_c1, by another name
subroutine common_shift()
    implicit none
    real :: h(4)
    common /asa10/ h
    h(2:4) = h(2:4) + h(1:3)
end subroutine

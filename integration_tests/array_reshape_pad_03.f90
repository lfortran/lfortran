program array_reshape_pad_03
    ! `pad` whose size is only known at run time (allocatable, pointer,
    ! array section, assumed-shape dummy) with a fixed-size `source`
    implicit none
    integer :: x(2,2), r2(2,2), r3(2,3), n
    integer, allocatable :: p(:), pm(:,:)
    integer, pointer :: q(:)
    integer, target :: t(4), m(3,2)
    integer(8) :: v8(2), r8(5)
    integer(8), allocatable :: p8(:)
    real(8) :: a(2,2), b(3,3)
    real(8), allocatable :: pa(:)
    logical :: l(2), rl(4)
    logical, allocatable :: pl(:)

    x = reshape([1, 2, 3, 4], [2, 2])

    ! Allocatable `pad`
    allocate(p(1))
    p = 0
    r2 = reshape([1, 2], [2, 2], pad=p)
    if (any(r2(:,1) /= [1, 2])) error stop
    if (any(r2(:,2) /= [0, 0])) error stop
    print *, reshape([1, 2], [2, 2], pad=p)
    deallocate(p)
    allocate(p(2))
    p = [5, 6]
    r3 = reshape(x, [2, 3], pad=p)
    if (any(r3(:,1) /= [1, 2])) error stop
    if (any(r3(:,2) /= [3, 4])) error stop
    if (any(r3(:,3) /= [5, 6])) error stop

    ! Allocatable `pad` of another rank
    allocate(pm(2,2))
    pm = reshape([8, 9, 10, 11], [2, 2])
    r3 = reshape(x, [2, 3], pad=pm)
    if (any(r3(:,2) /= [3, 4])) error stop
    if (any(r3(:,3) /= [8, 9])) error stop

    ! Pointer `pad`, also associated with a strided section
    allocate(q(2))
    q = [21, 22]
    r3 = reshape(x, [2, 3], pad=q)
    if (any(r3(:,2) /= [3, 4])) error stop
    if (any(r3(:,3) /= [21, 22])) error stop
    deallocate(q)
    t = [31, 0, 32, 0]
    q => t(1:3:2)
    r3 = reshape(x, [2, 3], pad=q)
    if (any(r3(:,3) /= [31, 32])) error stop
    q => t(3:1:-2)
    r3 = reshape(x, [2, 3], pad=q)
    if (any(r3(:,3) /= [32, 31])) error stop

    ! Array section `pad`
    n = 2
    r3 = reshape(x, [2, 3], pad=t(n+1:4))
    if (any(r3(:,3) /= [32, 0])) error stop
    r3 = reshape(x, [2, 3], pad=t(1:n+1:n))
    if (any(r3(:,3) /= [31, 32])) error stop

    ! Assumed-shape dummy `pad`, of any size and rank
    call check_rank1(t(1:2), [31, 0, 31])
    call check_rank1(t(1:3:2), [31, 32, 31])
    call check_rank1(pm(2,:), [9, 11, 9])
    call check_rank1(t, [31, 0, 32])
    call check_rank1(t(3:3), [32, 32, 32])
    m = reshape([41, 42, 43, 44, 45, 46], [3, 2])
    call check_rank2(m, [41, 42, 43])
    call check_rank2(m(1:3:2,:), [41, 43, 44])
    call check_rank2(m(2:2,:), [42, 45, 42])
    call check_rank2(pm, [8, 9, 10])

    ! Other types and kinds
    v8 = [10_8, 20_8]
    allocate(p8(2))
    p8 = [-5_8, -6_8]
    r8 = reshape(v8, [5], pad=p8)
    if (any(r8 /= [10_8, 20_8, -5_8, -6_8, -5_8])) error stop

    a = reshape([1.5d0, 2.5d0, 3.5d0, 4.5d0], [2, 2])
    allocate(pa(2))
    pa = [-1d0, -2d0]
    b = reshape(a, [3, 3], pad=pa)
    if (any(abs(b(:,1) - [1.5d0, 2.5d0, 3.5d0]) > 1d-12)) error stop
    if (any(abs(b(:,2) - [4.5d0, -1d0, -2d0]) > 1d-12)) error stop
    if (any(abs(b(:,3) - [-1d0, -2d0, -1d0]) > 1d-12)) error stop

    l = .true.
    allocate(pl(1))
    pl = .false.
    rl = reshape(l, [4], pad=pl)
    if (.not. all(rl(1:2))) error stop
    if (any(rl(3:4))) error stop

    ! In a loop, the reshape must not use more stack on every execution
    call check_loop(t(1:3:2), 1000000)

contains

    ! `pad` is taken from its first element on, in array element order, and
    ! repeated when it is shorter than the part of the result to fill
    subroutine check_rank1(d, expected)
        integer, intent(in) :: d(:), expected(3)
        integer :: w(3), r(2,3)
        w = [1, 2, 3]
        r = reshape(w, [2, 3], pad=d)
        if (any(r(:,1) /= [1, 2])) error stop
        if (r(1,2) /= 3) error stop
        if (any([r(2,2), r(:,3)] /= expected)) error stop
    end subroutine

    subroutine check_rank2(d, expected)
        integer, intent(in) :: d(:,:), expected(3)
        integer :: w(3), r(2,3)
        w = [1, 2, 3]
        r = reshape(w, [2, 3], pad=d)
        if (any(r(:,1) /= [1, 2])) error stop
        if (r(1,2) /= 3) error stop
        if (any([r(2,2), r(:,3)] /= expected)) error stop
    end subroutine

    subroutine check_loop(d, n)
        integer, intent(in) :: d(:), n
        integer :: w(2,2), r(2,3), i
        integer(8) :: s
        w = reshape([1, 2, 3, 4], [2, 2])
        s = 0
        do i = 1, n
            r = reshape(w, [2, 3], pad=d)
            s = s + r(1,3) - r(2,3)
        end do
        if (s /= -int(n, 8)) error stop
    end subroutine

end program

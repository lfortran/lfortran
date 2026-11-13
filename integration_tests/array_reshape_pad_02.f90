program array_reshape_pad_02
    ! `pad` only needs the type and kind of `source`: its rank and its
    ! allocatable or pointer attribute do not matter
    implicit none
    integer, parameter :: c(5) = reshape(reshape([1, 2, 3, 4], [2, 2]), [5], pad=[0])
    integer :: x(2,2), v(4), r1(5), r2(2,2), r3(2,3), p2(1,2)
    integer(8) :: x8(2,2), r8(6)
    real(8) :: a(2,2), b(3,2), pa(2)
    logical :: l(2,2), rl(5)
    integer, allocatable :: p(:), s(:)
    integer, pointer :: q(:,:)

    if (any(c /= [1, 2, 3, 4, 0])) error stop

    ! Rank-2 `source`, rank-1 `pad`
    x = reshape([1, 2, 3, 4], [2, 2])
    r1 = reshape(x, [5], pad=[0])
    if (any(r1 /= [1, 2, 3, 4, 0])) error stop
    print *, reshape(x, [5], pad=[0])

    ! Rank-1 `source`, rank-2 `pad`
    v = [1, 2, 3, 4]
    p2 = reshape([-1, -2], [1, 2])
    r3 = reshape(v, [2, 3], pad=p2)
    if (any(r3(:,1) /= [1, 2])) error stop
    if (any(r3(:,2) /= [3, 4])) error stop
    if (any(r3(:,3) /= [-1, -2])) error stop
    r3 = reshape([1, 2, 3, 4], [2, 3], pad=reshape([7, 8], [1, 2]))
    if (any(r3(:,2) /= [3, 4])) error stop
    if (any(r3(:,3) /= [7, 8])) error stop

    ! Other types and kinds
    x8 = reshape([10_8, 20_8, 30_8, 40_8], [2, 2])
    r8 = reshape(x8, [6], pad=[-5_8])
    if (any(r8 /= [10_8, 20_8, 30_8, 40_8, -5_8, -5_8])) error stop

    a = reshape([1.5d0, 2.5d0, 3.5d0, 4.5d0], [2, 2])
    pa = [-1d0, -2d0]
    b = reshape(a, [3, 2], pad=pa)
    if (any(abs(b(:,1) - [1.5d0, 2.5d0, 3.5d0]) > 1d-12)) error stop
    if (any(abs(b(:,2) - [4.5d0, -1d0, -2d0]) > 1d-12)) error stop

    l = .true.
    rl = reshape(l, [5], pad=[.false.])
    if (.not. all(rl(1:4))) error stop
    if (rl(5)) error stop

    ! Allocatable or pointer `pad`, non-allocatable `source`
    allocate(p(1), q(1,1))
    p = 0
    q = 0
    r2 = reshape([1, 2, 3, 4], [2, 2], pad=p)
    if (any(r2(:,1) /= [1, 2])) error stop
    if (any(r2(:,2) /= [3, 4])) error stop
    r2 = reshape(v, [2, 2], pad=q)
    if (any(r2(:,1) /= [1, 2])) error stop
    if (any(r2(:,2) /= [3, 4])) error stop
    if (size(reshape([1, 2], [2, 2], pad=p)) /= 4) error stop
    deallocate(q)

    ! Allocatable `source`, non-allocatable `pad`
    allocate(s(4))
    s = [5, 6, 7, 8]
    r2 = reshape(s, [2, 2], pad=[0])
    if (any(r2(:,1) /= [5, 6])) error stop
    if (any(r2(:,2) /= [7, 8])) error stop
    r2 = reshape(s, [2, 2], pad=p2)
    if (any(r2(:,1) /= [5, 6])) error stop
    if (any(r2(:,2) /= [7, 8])) error stop
end program

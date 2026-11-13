program array_reshape_pad_04
    ! `pad` is taken from its first element on, also when the size of
    ! `source` is not a multiple of the size of `pad`
    implicit none
    integer :: s(3), p(2), pm(2,2), r(2,3), r7(7)
    real(8) :: a(1), b(4), pa(3)
    logical :: l(1), rl(3), pl(2)

    s = [1, 2, 3]
    p = [7, 8]

    r = reshape(s, [2, 3], pad=p)
    if (any(r(:,1) /= [1, 2])) error stop
    if (any(r(:,2) /= [3, 7])) error stop
    if (any(r(:,3) /= [8, 7])) error stop

    r = reshape(s, [2, 3], pad=[7, 8])
    if (any(r(:,2) /= [3, 7])) error stop
    if (any(r(:,3) /= [8, 7])) error stop

    r = reshape([1, 2, 3], [2, 3], pad=p)
    if (any(r(:,2) /= [3, 7])) error stop
    if (any(r(:,3) /= [8, 7])) error stop

    r7 = reshape(s, [7], pad=p)
    if (any(r7 /= [1, 2, 3, 7, 8, 7, 8])) error stop

    pm = reshape([11, 12, 13, 14], [2, 2])
    r = reshape(s, [2, 3], pad=pm)
    if (any(r(:,2) /= [3, 11])) error stop
    if (any(r(:,3) /= [12, 13])) error stop

    a = [1.5d0]
    pa = [-1d0, -2d0, -3d0]
    b = reshape(a, [4], pad=pa)
    if (any(abs(b - [1.5d0, -1d0, -2d0, -3d0]) > 1d-12)) error stop

    l = .true.
    pl = [.false., .true.]
    rl = reshape(l, [3], pad=pl)
    if (.not. rl(1)) error stop
    if (rl(2)) error stop
    if (.not. rl(3)) error stop
end program

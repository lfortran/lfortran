program array_reshape_pad_02
    ! `pad` may have a different rank than `source`
    implicit none
    type :: t
        integer :: i
    end type
    real :: r(2, 3), r1(8)
    integer :: n(2, 2), n1(3, 2)
    real(8) :: d(2, 3, 1), d1(8)
    type(t) :: s(2, 3), s1(8)
    real :: p(3)
    integer :: k

    r = reshape([1., 2., 3., 4., 5., 6.], [2, 3])
    r1 = reshape(r, [8], pad=[0.])
    if (any(r1 /= [1., 2., 3., 4., 5., 6., 0., 0.])) error stop

    n = reshape([1, 2, 3, 4], [2, 2])
    n1 = reshape(n, [3, 2], pad=[7, 8])
    if (any(reshape(n1, [6]) /= [1, 2, 3, 4, 7, 8])) error stop

    d = reshape([1d0, 2d0, 3d0, 4d0, 5d0, 6d0], [2, 3, 1])
    d1 = reshape(d, [8], pad=[-1d0])
    if (any(d1 /= [1d0, 2d0, 3d0, 4d0, 5d0, 6d0, -1d0, -1d0])) error stop

    s = reshape([(t(k), k = 1, 6)], [2, 3])
    s1 = reshape(s, [8], pad=[t(0)])
    if (any(s1%i /= [1, 2, 3, 4, 5, 6, 0, 0])) error stop

    p = [7., 8., 9.]
    r1 = reshape(r, [8], pad=p)
    if (any(r1 /= [1., 2., 3., 4., 5., 6., 7., 8.])) error stop

    print *, r1
    print *, s1%i
end program

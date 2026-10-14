! gpu decline reason: AliasTemporaryRuntimeSized
! an array assignment whose two sides overlap needs a temporary. Its extent
! `m(k)` changes with the iteration, so the per-thread temporary would be
! sized as the whole component `x(k)%v` instead, but that is an element of
! an array of derived type whose size may differ from one iteration to the
! next, so the host cannot size it before the launch either.
program gpu_decline_01
    implicit none
    type :: t_t
        real, allocatable :: v(:)
    end type
    type(t_t) :: x(3)
    integer :: m(3), k, i
    m = [2, 3, 1]
    do k = 1, 3
        allocate(x(k)%v(m(k)))
        x(k)%v = [(real(i), i = 1, m(k))]
    end do
    do concurrent (k = 1:3)
        x(k)%v(1:m(k)) = x(k)%v(m(k):1:-1)
    end do
    if (any(x(1)%v /= [2., 1.])) error stop "bad x(1)"
    if (any(x(2)%v /= [3., 2., 1.])) error stop "bad x(2)"
    if (any(x(3)%v /= [1.])) error stop "bad x(3)"
end program

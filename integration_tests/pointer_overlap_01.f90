program pointer_overlap_01
    ! The value of an assignment, computed into its target, reads the
    ! target through a pointer: it is evaluated before the target is
    ! defined.
    implicit none
    real, target :: e(4)
    real, allocatable, target :: x(:)
    real, pointer :: q(:)
    real :: m(4, 4)
    integer :: i

    e = [1., 2., 3., 4.]
    q => e
    e = cshift(q, 1)
    if (any(e /= [2., 3., 4., 1.])) error stop 1

    e = [1., 2., 3., 4.]
    q => e
    e = eoshift(q, -1)
    if (any(e /= [0., 1., 2., 3.])) error stop 2

    allocate(x(4))
    x = [1., 2., 3., 4.]
    q => x
    x = cshift(q, -1)
    if (any(x /= [4., 1., 2., 3.])) error stop 3

    m = 0
    do i = 1, 4
        m(i, 5 - i) = 1
    end do
    x = [1., 2., 3., 4.]
    q => x
    x = matmul(m, q)
    if (any(x /= [4., 3., 2., 1.])) error stop 4

    e = [1., 2., 3., 4.]
    q => e
    e = reversed(q)
    if (any(e /= [4., 3., 2., 1.])) error stop 5

    e = [1., 2., 3., 4.]
    q => e(4:1:-1)
    e = cshift(q, 1)
    if (any(e /= [3., 2., 1., 4.])) error stop 6
    print *, e

contains

    function reversed(a) result(r)
        real, intent(in) :: a(:)
        real :: r(size(a))
        integer :: k
        do k = 1, size(a)
            r(k) = a(size(a) + 1 - k)
        end do
    end function

end program pointer_overlap_01

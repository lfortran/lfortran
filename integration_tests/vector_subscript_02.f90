program vector_subscript_02
    ! A vector subscript that is an elemental expression rather than an
    ! array variable: an elemental intrinsic, a conversion, a unary minus
    ! or a user elemental function applied to an index array (#17225)
    implicit none
    type :: t
        integer :: n
    end type
    type(t) :: c(5)
    integer :: idx(3), neg(3), vals(5), picked(3), m(3, 3), q(2, 2), i, j
    integer :: idx0(0:2)
    integer(8) :: idx8(3)
    integer, allocatable :: alloc(:)
    real :: r(3), v(5), rq(3)

    vals = [10, 20, 30, 40, 50]
    idx = [-4, 2, -1]
    picked = vals(abs(idx))
    if (any(picked /= [40, 20, 10])) error stop 1

    v = [1.5, 2.5, 3.5, 4.5, 5.5]
    r = [4.7, 2.2, 1.9]
    rq = v(int(r))
    if (any(rq /= [4.5, 2.5, 1.5])) error stop 2

    allocate(alloc(3))
    alloc = vals(nint(r))
    if (size(alloc) /= 3) error stop 3
    if (any(alloc /= [50, 20, 20])) error stop 4

    neg = [-4, -2, -1]
    picked = vals(-neg)
    if (any(picked /= [40, 20, 10])) error stop 5

    picked = vals(max(idx, 1))
    if (any(picked /= [10, 20, 10])) error stop 6

    picked = vals(half(abs(idx) * 2))
    if (any(picked /= [40, 20, 10])) error stop 7

    if (sum(vals(abs(idx))) /= 70) error stop 8
    call check(vals(abs(idx)), [40, 20, 10], 9)

    idx0 = [-4, 2, -1]
    picked = vals(abs(idx0))
    if (any(picked /= [40, 20, 10])) error stop 10

    idx8 = [-4_8, 2_8, -1_8]
    picked = vals(abs(idx8))
    if (any(picked /= [40, 20, 10])) error stop 11

    vals = 0
    vals(abs(idx)) = [7, 8, 9]
    if (any(vals /= [9, 8, 0, 7, 0])) error stop 12

    do j = 1, 3
        do i = 1, 3
            m(i, j) = 10*i + j
        end do
    end do
    picked(1:2) = m(abs(idx(3:2:-1)), 2)
    if (any(picked(1:2) /= [12, 22])) error stop 13
    q = m(abs(idx(3:2:-1)), 2:3)
    if (any(q(:, 1) /= [12, 22]) .or. any(q(:, 2) /= [13, 23])) error stop 14

    do i = 1, 5
        c(i)%n = 10*i
    end do
    picked = c(abs(idx))%n
    if (any(picked /= [40, 20, 10])) error stop 15
    call check_class(c, idx)

    vals = [10, 20, 30, 40, 50]
    picked = 0
    where (vals(abs(idx)) > 15) picked = 1
    if (any(picked /= [1, 1, 0])) error stop 17
    forall (i = 1:3) picked(i) = sum(vals(abs(idx(1:i))))
    if (any(picked /= [40, 60, 70])) error stop 18
    if (maxval(vals(abs(idx))) /= 40) error stop 19
    print *, "OK"

contains

    elemental integer function half(k)
        integer, intent(in) :: k
        half = k/2
    end function

    subroutine check_class(p, k)
        class(t), intent(in) :: p(:)
        integer, intent(in) :: k(:)
        integer :: got(3)
        got = p(abs(k))%n
        if (any(got /= [40, 20, 10])) error stop 16
    end subroutine

    subroutine check(a, expected, code)
        integer, intent(in) :: a(:), expected(:), code
        if (any(a /= expected)) error stop code
    end subroutine

end program

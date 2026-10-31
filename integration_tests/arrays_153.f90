module arrays_153_procedures
    implicit none
    integer :: calls
contains
    real function first(v)
        real, intent(in) :: v(:)
        first = v(1)
    end function

    real(8) function weighted(v)
        real(8), intent(in) :: v(:)
        integer :: i
        weighted = 0.0_8
        do i = 1, size(v)
            weighted = weighted + i * v(i)
        end do
    end function

    subroutine inspect(v, value)
        real(8), intent(in) :: v(:,:)
        real(8), intent(out) :: value
        integer :: i, j
        value = 0.0_8
        do j = 1, size(v,2)
            do i = 1, size(v,1)
                value = value + (i + 10*j) * v(i,j)
            end do
        end do
    end subroutine

    function twice(v) result(r)
        real(8), intent(in) :: v(:)
        real(8) :: r(size(v))
        integer :: i
        do i = 1, size(v)
            r(i) = 2.0_8 * v(i)
        end do
    end function

    integer function select_index(i) result(j)
        integer, intent(in) :: i
        calls = calls + 1
        j = i
    end function
end module

program arrays_153
    use arrays_153_procedures
    implicit none
    real :: v(1,1)
    real(8) :: a(-2:3), m(-1:1,3:4), value, r(3)
    integer :: i, j, lo, hi, stride, repeat

    v(1,1) = 2.0
    if (first(v(:,1)) /= 2.0) error stop
    do i = -2, 3
        a(i) = i * 1.0_8
    end do
    do j = 3, 4
        do i = -1, 1
            m(i,j) = (10*j+i) * 1.0_8
        end do
    end do

    if (weighted(a(:)) /= 28.0_8) error stop
    if (weighted(a(-2:2:2)) /= 4.0_8) error stop
    if (weighted(a(3:-1:-2)) /= 2.0_8) error stop
    if (weighted(m(:,3)) /= 182.0_8) error stop
    if (weighted(m(0,4:3:-1)) /= 100.0_8) error stop
    call inspect(m(1:-1:-1,4:3:-1), value)
    if (value /= 3416.0_8) error stop
    r = twice(a(3:-1:-2))
    if (r(1) /= 6.0_8 .or. r(2) /= 2.0_8 .or. r(3) /= -2.0_8) error stop

    lo = 3
    hi = -1
    stride = -2
    do repeat = 1, 3
        calls = 0
        value = weighted(a(select_index(lo):select_index(hi):select_index(stride)))
        if (calls /= 3 .or. value /= 2.0_8) error stop
    end do
    calls = 0
    value = weighted(m(select_index(0),select_index(4):select_index(3):-1))
    if (calls /= 3 .or. value /= 100.0_8) error stop

    lo = 2
    hi = -1
    stride = 2
    if (weighted(a(lo:hi:stride)) /= 0.0_8) error stop
    if (weighted(a(hi:lo:-stride)) /= 0.0_8) error stop
    hi = 1
    if (weighted(a(lo:hi:stride)) /= 0.0_8) error stop
    if (weighted(a(hi:lo:-stride)) /= 0.0_8) error stop
    call inspect(m(1:-1:1,:), value)
    if (value /= 0.0_8) error stop

    do i = -2, 3
        if (a(i) /= i * 1.0_8) error stop
    end do
    do j = 3, 4
        do i = -1, 1
            if (m(i,j) /= (10*j+i) * 1.0_8) error stop
        end do
    end do
    if (v(1,1) /= 2.0) error stop
end program

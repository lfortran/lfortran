module arrays_152_selectors
    implicit none
    integer :: calls
contains
    function select_index(i) result(j)
        integer, intent(in) :: i
        integer :: j
        calls = calls + 1
        j = i
    end function
end module

program arrays_152
    use arrays_152_selectors
    implicit none
    real :: a(1), b(2)
    real(8) :: x(-2:3), y(4:6), m(-1:1,3:4), n(5:7,-2:-1)
    integer :: i, j, lo, hi, stride, controls(4), repeat

    b(1) = 2.0
    a = b(1:1)
    if (a(1) /= 2.0) error stop

    do i = -2, 3
        x(i) = i * 1.0_8
    end do
    y = x(-2:2:2)
    if (y(4) /= -2.0_8 .or. y(5) /= 0.0_8 .or. y(6) /= 2.0_8) error stop
    y = x(3:-1:-2)
    if (y(4) /= 3.0_8 .or. y(5) /= 1.0_8 .or. y(6) /= -1.0_8) error stop

    do j = 3, 4
        do i = -1, 1
            m(i,j) = (10*j+i) * 1.0_8
        end do
    end do
    n = m(1:-1:-1,4:3:-1)
    if (n(5,-2) /= 41.0_8 .or. n(7,-2) /= 39.0_8) error stop
    if (n(5,-1) /= 31.0_8 .or. n(7,-1) /= 29.0_8) error stop
    n(6:5,:) = m(0:-1,:)
    if (n(5,-2) /= 41.0_8 .or. n(7,-1) /= 29.0_8) error stop
    y = m(:,3)
    if (y(4) /= 29.0_8 .or. y(5) /= 30.0_8 .or. y(6) /= 31.0_8) error stop
    y(4:5) = m(0,4:3:-1) + x(0:1)
    if (y(4) /= 40.0_8 .or. y(5) /= 31.0_8) error stop
    calls = 0
    y(4:5) = m(select_index(0),select_index(4):select_index(3):-1)
    if (calls /= 3 .or. y(4) /= 40.0_8 .or. y(5) /= 30.0_8) error stop

    lo = 3
    hi = -1
    stride = -2
    do repeat = 1, 3
        calls = 0
        y = x(select_index(lo):select_index(hi):select_index(stride))
        if (calls /= 3) error stop
        if (y(4) /= 3.0_8 .or. y(5) /= 1.0_8 .or. y(6) /= -1.0_8) error stop
    end do
    lo = 2
    hi = -1
    y(5:4) = x(lo:hi)
    y(5:4) = x(lo:hi:2)
    y(5:4) = x(hi:lo:-2)
    hi = 1
    y(5:4) = x(lo:hi:2)
    y(5:4) = x(hi:lo:-2)
    if (y(4) /= 3.0_8 .or. y(5) /= 1.0_8 .or. y(6) /= -1.0_8) error stop

    x = x(3:-2:-1)
    do i = -2, 3
        if (x(i) /= (1-i) * 1.0_8) error stop
    end do
    x(-1:3) = x(-2:2)
    do i = -1, 3
        if (x(i) /= (2-i) * 1.0_8) error stop
    end do
    controls(1) = 2
    controls(2) = 3
    controls(3) = 4
    controls(4) = 1
    controls = controls(controls(3):controls(4):-1)
    if (controls(1) /= 1 .or. controls(2) /= 4) error stop
    if (controls(3) /= 3 .or. controls(4) /= 2) error stop
end program

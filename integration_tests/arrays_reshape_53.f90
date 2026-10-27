! Assigning a constant RESHAPE to arrays whose lower bounds are not 1
program arrays_reshape_53
    implicit none
    integer :: b(0:5), c(0:1,0:2), i, j
    integer, allocatable :: a(:,:)
    real :: r(-2:-1,3:5)

    b = -1
    b = reshape([1,2,3,4,5,6], [6])
    do i = 0, 5
        if (b(i) /= i + 1) error stop
    end do

    c = reshape([1,2,3,4,5,6], [2,3])
    do j = 0, 2
        do i = 0, 1
            if (c(i,j) /= 1 + i + 2*j) error stop
        end do
    end do

    allocate(a(0:2,2))
    a = reshape([1,2,3,4,5,6], [3,2])
    if (lbound(a, 1) /= 0 .or. lbound(a, 2) /= 1) error stop
    do j = 1, 2
        do i = 0, 2
            if (a(i,j) /= 1 + i + 3*(j-1)) error stop
        end do
    end do

    r = reshape([1.0,2.0,3.0,4.0,5.0,6.0], [2,3])
    if (any(r(:,3) /= [1.0,2.0])) error stop
    if (any(r(:,5) /= [5.0,6.0])) error stop

    call explicit_shape(b, -3)
    if (any(b /= [6,5,4,3,2,1])) error stop

    call assumed_shape(c)
    if (any(c(:,0) /= [7,8])) error stop
    if (any(c(:,2) /= [11,12])) error stop
    print *, b
    print *, c

contains

    subroutine explicit_shape(x, n)
        integer, intent(in) :: n
        integer, intent(out) :: x(n:n+5)
        x = reshape([6,5,4,3,2,1], [6])
        if (x(n) /= 6 .or. x(n+5) /= 1) error stop
    end subroutine

    subroutine assumed_shape(y)
        integer, intent(out) :: y(-1:,:)
        y = reshape([7,8,9,10,11,12], [2,3])
        if (y(-1,1) /= 7 .or. y(0,3) /= 12) error stop
    end subroutine

end program

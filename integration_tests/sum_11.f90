program sum_11
    implicit none
    real :: a(4,2), result(2)
    real(8) :: wide(-2:1), value
    integer :: i, expected(0:4)
    do i = 1,4
        a(i,:) = real(i)
    end do
    if (twice(sum(a*2)) /= 80) error stop 1
    if (twice(sum(a(:,1)*2,mask=a(:,1)>2)) /= 28) error stop 2
    if (twice(sum(a(:,1)*2,mask=.false.)) /= 0) error stop 3
    if (twice(sum(a(:0,1)*2)) /= 0) error stop 4
    if (negate(any(a(:,1)*2<0))) then
    else
        error stop 5
    end if
    if (twice(sum(a(:,1)*2,dim=1)) /= 40) error stop 6
    result = twice(sum(a*2,dim=1))
    if (any(result /= 40)) error stop 7
    if (array_total(sum(a*2,dim=1)) /= 40) error stop 8
    wide = [0.25_8, 0.5_8, -0.75_8, 1.125_8]
    expected = [-1, 0, 1, 0, 2]
    value = -1
    do i = 0,4
        if (i > 0) value = rounded(sum(wide(-2:i-3)*2))
        if (value /= expected(i)) error stop 9
    end do
    print *, "ok"
contains
    pure elemental real function twice(x) result(r)
        real, intent(in) :: x
        r = 2*x
    end function
    pure elemental logical function negate(x) result(r)
        logical, intent(in) :: x
        r = .not. x
    end function
    pure real function array_total(x) result(r)
        real, intent(in) :: x(:)
        r = sum(x)
    end function
    pure real(8) function rounded(x) result(r)
        real(8), intent(in) :: x
        r = real(int(x),8)
    end function
end program

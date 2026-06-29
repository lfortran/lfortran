program test
    integer :: i
    real :: mx

    mx = -1.0

    do concurrent (i=1:10) reduce(max:mx)
        mx = max(mx, real(i))
    end do
    if(mx /= 10.0) stop "incorrect result"
    print *, mx
end program
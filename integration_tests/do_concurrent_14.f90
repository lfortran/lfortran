program do_concurrent_14

    implicit none
    real :: a(1000000), b(1000000)
    integer :: i
    b = 5
    a = 0
    do concurrent (i = 1:1000000) shared(a, b)
        a(i) = i + b(i)*340
    end do

    print*, a(5), b(5)

    if(a(5) /= 1705) error stop
    if(b(5) /= 5) error stop
end program do_concurrent_14

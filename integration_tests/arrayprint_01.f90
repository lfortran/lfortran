program arrayprint_01
    real :: x(3)
    integer :: i = 3
    do i=1,3
     x(i) = i
    end do
    print*, x
end program
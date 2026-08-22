program do_concurrent_16
    implicit none

    integer :: i
    integer :: values(3)

    i = 10
    values = 0

    do concurrent (integer :: i = 1:3)
        values(i) = i * i
    end do

    if (any(values /= [1, 4, 9])) error stop 1
    if (i /= 10) error stop 2
end program do_concurrent_16
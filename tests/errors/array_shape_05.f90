program array_shape_05
    implicit none
    integer :: a(2)
    integer :: temp(5)
    integer :: i

    call system_clock(count=i)
    i = 1 + mod(i, size(temp))
    a = temp(i:i)
end program

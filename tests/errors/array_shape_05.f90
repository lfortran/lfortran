program array_shape_05
    implicit none
    integer :: a(2)
    integer :: temp(5)
    integer :: i
    integer :: j

    call system_clock(count=i)
    i = 1 + mod(i, size(temp))
    j = i
    a = temp(i:j)
end program

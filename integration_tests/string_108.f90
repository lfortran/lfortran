program string_108
    implicit none
    character(:), allocatable :: result
    integer :: i, n
    allocate(character(4) :: result)
    do i = 1, 4
        n = i
        result(i:i) = '0123456789abcdef'(n:n)
    end do
    if (result /= '0123') error stop
    print *, result
end program string_108

program arrays_158
    implicit none
    integer, allocatable :: a(:)
    integer :: i, j, k, n
    a = [2,3,5]
    do k = 1, 4
        n = size(a)
        a = [a, ((a(i)+j,j=1,a(i)*a(i)),i=1,n)]
        if (size(a) <= n) error stop 1
        if (any(a(1:3) /= [2,3,5])) error stop 2
        if (a(n+1) /= 3 .or. a(n+4) /= 6) error stop 3
        if (k == 1) then
            if (size(a) /= 41) error stop 4
            if (a(41) /= 30) error stop 5
        end if
        if (k < 4) a = a(1:3)
    end do
end program

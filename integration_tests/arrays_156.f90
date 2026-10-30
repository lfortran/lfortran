module constructor_nested_controls
    implicit none
    integer :: count1, count2, count3, count4, outer_calls
contains
    integer function limit(i)
        integer, intent(in) :: i
        select case (i)
        case (1)
            count1 = count1 + 1
        case (2)
            count2 = count2 + 1
        case (3)
            count3 = count3 + 1
        case (4)
            count4 = count4 + 1
        end select
        limit = i*i - 1
    end function
    integer function outer()
        outer_calls = outer_calls + 1
        outer = 4
    end function
end module

program arrays_156
    use constructor_nested_controls
    implicit none
    integer, allocatable :: a(:)
    integer :: i, j, k, p, expected(26), counts(4)
    count1 = 0
    count2 = 0
    count3 = 0
    count4 = 0
    outer_calls = 0
    p = 0
    do i = 1, 4
        do j = 1, i*i-1
            p = p + 1
            expected(p) = 10*i + j
        end do
    end do
    do k = 1, 3
        a = [((10*i+j, j=1,limit(i)), i=1,outer())]
        counts = [count1,count2,count3,count4]
        if (any(counts /= k) .or. outer_calls /= k) error stop 1
        if (size(a) /= 26) error stop 2
        if (any(a /= expected)) error stop 3
    end do
    a = [((j,j=1,limit(i)),i=1,0)]
    counts = [count1,count2,count3,count4]
    if (any(counts /= 3)) error stop 4
    if (size(a) /= 0) error stop 5
end program

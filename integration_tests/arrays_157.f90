module constructor_array_leaves
    implicit none
    integer :: calls1, calls2, calls3, bounds, dynamic1, dynamic2, dynamic3
contains
    function values(i) result(a)
        integer, intent(in) :: i
        integer :: a(-1:0,2:3)
        select case (i)
        case (1)
            calls1 = calls1 + 1
        case (2)
            calls2 = calls2 + 1
        case (3)
            calls3 = calls3 + 1
        end select
        a(-1,2) = 10*i+1
        a(0,2) = 10*i+2
        a(-1,3) = 10*i+3
        a(0,3) = 10*i+4
    end function
    integer function limit()
        bounds = bounds + 1
        limit = 3
    end function
    function dynamic_values(i) result(a)
        integer, intent(in) :: i
        integer, allocatable :: a(:)
        integer :: j
        select case (i)
        case (1)
            dynamic1 = dynamic1 + 1
        case (2)
            dynamic2 = dynamic2 + 1
        case (3)
            dynamic3 = dynamic3 + 1
        end select
        allocate(a(0:i-1))
        do j = 0, i-1
            a(j) = 10*i + j
        end do
    end function
end module

program arrays_157
    use constructor_array_leaves
    implicit none
    integer :: a(12), i, j, k, calls(3), b(6)
    calls1 = 0
    calls2 = 0
    calls3 = 0
    bounds = 0
    dynamic1 = 0
    dynamic2 = 0
    dynamic3 = 0
    do k = 1, 2
        a = [(values(i),i=1,limit())]
        calls = [calls1,calls2,calls3]
        if (any(calls /= k) .or. bounds /= k) error stop 1
        if (any(a /= [11,12,13,14,21,22,23,24,31,32,33,34])) error stop 2
    end do
    a = [(values(i),i=1,3)]
    calls = [calls1,calls2,calls3]
    if (any(calls /= 3) .or. bounds /= 2) error stop 3
    if (any(a /= [11,12,13,14,21,22,23,24,31,32,33,34])) error stop 4
    b = [(dynamic_values(i),i=1,3)]
    calls = [dynamic1,dynamic2,dynamic3]
    if (any(calls /= 1)) error stop 5
    if (any(b /= [10,20,21,30,31,32])) error stop 6
    b = [(dynamic_values(i) + [(j,j=1,i)],i=1,3)]
    calls = [dynamic1,dynamic2,dynamic3]
    if (any(calls /= 2)) error stop 7
    if (any(b /= [11,21,23,31,33,35])) error stop 8
end program

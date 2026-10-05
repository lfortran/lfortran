module traits_recursive_01_oracle_m
    implicit none
    type :: Box
        integer :: data
    end type
contains
    function box_value(self) result(r)
        class(Box), intent(in) :: self
        integer :: r
        r = self%data
    end function
    recursive function first(x, n) result(r)
        type(Box), intent(in) :: x
        integer, intent(in) :: n
        integer :: r
        if (n == 0) then
            r = box_value(x)
        else
            r = second(x, n-1) + 1
        end if
    end function
    recursive function second(x, n) result(r)
        type(Box), intent(in) :: x
        integer, intent(in) :: n
        integer :: r
        if (n == 0) then
            r = box_value(x)
        else
            r = first(x, n-1) + 1
        end if
    end function
    recursive function total(x, n) result(r)
        type(Box), intent(in) :: x
        integer, intent(in) :: n
        integer :: r
        if (n == 0) then
            r = box_value(x)
        else
            r = total(x, n-1) + 1
        end if
    end function
end module
program traits_recursive_01_oracle
    use traits_recursive_01_oracle_m
    implicit none
    type(Box) :: x, y
    integer :: n
    x = Box(11)
    y = Box(111)
    print *, first(x, 0), first(x, 1), first(x, 2), first(x, 3), first(x, 4)
    print *, second(x, 0), second(x, 1), second(x, 2), second(x, 3), second(x, 4)
    do n = 0, 8
        if (first(x, n) /= 11+n) error stop 1
        if (second(x, n) /= 11+n) error stop 2
        if (first(y, n) /= 111+n) error stop 3
        if (second(y, n) /= 111+n) error stop 4
        if (total(x, n) /= 11+n) error stop 5
        if (total(y, n) /= 111+n) error stop 6
    end do
end program

program inline_scoped_01
    implicit none
    integer :: value, total, i
    value = 2
    total = 0
    do i = 1, 3
        total = total + advance(value)
    end do
    if (value /= 5) error stop 1
    if (total /= 24) error stop 2
contains
    integer function advance(value) result(next)
        integer, intent(inout) :: value
        integer :: total
        total = 2
        value = value + 1
        next = total * value
    end function
end program

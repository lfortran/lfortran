program where_06
    implicit none
    integer :: x(10)
    x = [38, 79, -54, 13, -66, 77, -62, 39, 6, -3]
    if (get_positive_sum(x) /= 252) error stop
contains
    integer function get_positive_sum(input_1, input_2) result(output)
        integer, intent(in) :: input_1(:)
        integer, intent(in), optional :: input_2
        integer :: x(size(input_1))
        x = input_1
        where(input_1 < 0) x = 0
        output = sum(x)
    end function get_positive_sum
end program where_06

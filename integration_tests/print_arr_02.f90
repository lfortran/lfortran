program print_arr_with_selectcase
    implicit none
    integer :: i, out, m
    integer :: x(3) = [1, 2, 3], y(3) = [4, 5, 6]
    integer :: x1(3) = [10, 20, 30], y1(3) = [40, 50, 50]
    integer, parameter :: a = 1, b = 2
    i = 2
    m = 45
    select case(i)
        case (1)
            print *, x
            out = x(1)
        case (2)
            print *, y
            out = y(1)
        case (3)
            print *, "3"
            out = -1
    end select
    if (out /= 4) error stop

    select case (m)

       case ((40 + b):)
          print *, x1
          out = x1(1)

       case (:(39 - a))
          print *, y1
          out = y1(1)

       case default
          print *, "Invalid"
          out = -1
    end select

    if (out /= 10) error stop
end

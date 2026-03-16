program array_section_19
    implicit none

    integer, parameter :: a(5) = [10, 20, 30, 40, 50]
    integer, parameter :: b(3) = a(2:4)
    integer, parameter :: c(5) = a(1:5)
    integer, parameter :: d(2) = a(1:3:2)

    integer, parameter :: array(4) = [1, 2, 3, 4]
    integer, parameter :: arr_int(4) = array(1:4)

    character, parameter :: string(4) = "okay"
    character, parameter :: arr(4) = string(1:4)

    if (b(1) /= 20) error stop
    if (b(2) /= 30) error stop
    if (b(3) /= 40) error stop

    if (c(1) /= 10) error stop
    if (c(2) /= 20) error stop
    if (c(3) /= 30) error stop
    if (c(4) /= 40) error stop
    if (c(5) /= 50) error stop

    if (d(1) /= 10) error stop
    if (d(2) /= 30) error stop

    if (arr_int(1) /= 1) error stop
    if (arr_int(2) /= 2) error stop
    if (arr_int(3) /= 3) error stop
    if (arr_int(4) /= 4) error stop

    if (arr(1) /= "o") error stop
    if (arr(2) /= "o") error stop
    if (arr(3) /= "o") error stop
    if (arr(4) /= "o") error stop
end program array_section_19

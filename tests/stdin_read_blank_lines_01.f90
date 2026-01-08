program stdin_read_blank_lines_01
    implicit none

    integer :: i
    integer :: m
    integer :: n
    integer, parameter :: expected_m(2) = [10, 20]
    integer, parameter :: expected_n(2) = [5, 10]

    do i = 1, 2
        read(*, *) m
        read(*, *) n
        if (m /= expected_m(i)) error stop 1
        if (n /= expected_n(i)) error stop 1
    end do
end program stdin_read_blank_lines_01


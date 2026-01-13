program transfer_string_to_int8
    ! Test transfer of character string to integer(1) array
    implicit none
    integer(1) :: bytes(5)
    character(len=5) :: str
    integer :: i
    integer(1), parameter :: expected(5) = [104_1, 101_1, 108_1, 108_1, 111_1]  ! "hello"

    str = "hello"
    bytes = transfer(str, bytes)

    do i = 1, 5
        if (bytes(i) /= expected(i)) error stop
    end do

    print *, "PASS"
end program

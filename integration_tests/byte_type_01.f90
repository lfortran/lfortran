! Test the non-standard `byte` type, which is equivalent to integer(1).
program byte_type_01
    implicit none
    byte :: b
    byte :: arr(3)
    byte, parameter :: c = 7
    integer :: i
    integer :: byte

    ! Scalar behaves like integer(1)
    b = 5
    if (kind(b) /= 1) error stop
    if (storage_size(b) /= 8) error stop
    if (b /= 5) error stop

    ! Named constant
    if (c /= 7) error stop

    ! Array of byte
    do i = 1, 3
        arr(i) = int(i, kind=1)
    end do
    if (sum(arr) /= 6) error stop
    if (arr(2) /= 2) error stop

    ! `byte` is not a reserved word and still works as a variable name
    byte = 42
    if (byte /= 42) error stop

    print *, b, sum(arr), c, byte
end program byte_type_01

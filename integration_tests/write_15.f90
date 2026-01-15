program write_15
    ! Test implied-do loops with strings in write statements
    implicit none
    character(len=10) :: lines(3)
    integer :: i, unit_num

    lines(1) = "Hello"
    lines(2) = "World"
    lines(3) = "Test"

    ! Open a file for writing
    unit_num = 10
    open(unit=unit_num, file="write_15_output.txt", status="replace", action="write")

    ! Write using implied-do loop with trim
    write(unit_num, '(A)') (trim(lines(i)), i=1,3)

    close(unit_num)

    ! Read back and verify
    open(unit=unit_num, file="write_15_output.txt", status="old", action="read")

    do i = 1, 3
        read(unit_num, '(A)') lines(i)
    end do

    close(unit_num, status="delete")

    if (trim(lines(1)) /= "Hello") error stop "Line 1 mismatch"
    if (trim(lines(2)) /= "World") error stop "Line 2 mismatch"
    if (trim(lines(3)) /= "Test") error stop "Line 3 mismatch"

    print *, "PASS"
end program write_15

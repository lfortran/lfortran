! Test that embedded null bytes in strings are preserved during formatting
program format_45
    implicit none
    character(len=20) :: result_str
    character(len=10) :: prefix
    real :: value
    integer :: i

    ! Set prefix to null bytes
    do i = 1, 10
        prefix(i:i) = char(0)
    end do
    value = 1.5

    write(result_str, '(A,F5.2)') prefix, value

    ! Verify first 10 bytes are null
    do i = 1, 10
        if (ichar(result_str(i:i)) /= 0) error stop
    end do

    ! Verify float " 1.50" at positions 11-15
    if (ichar(result_str(11:11)) /= 32) error stop  ! space
    if (ichar(result_str(12:12)) /= 49) error stop  ! '1'
    if (ichar(result_str(13:13)) /= 46) error stop  ! '.'
    if (ichar(result_str(14:14)) /= 53) error stop  ! '5'
    if (ichar(result_str(15:15)) /= 48) error stop  ! '0'

    ! Verify trailing spaces at positions 16-20
    do i = 16, 20
        if (ichar(result_str(i:i)) /= 32) error stop
    end do

    print *, "PASS: Null bytes preserved correctly in formatted output"
end program

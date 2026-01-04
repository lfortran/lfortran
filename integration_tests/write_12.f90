program write_12
    implicit none
    integer :: x
    character(20) :: buf

    x = 42

    ! Test that unit 6 is pre-connected (writes to stdout)
    write(6, '(A,I0)') "x = ", x

    ! Verify formatted write works correctly
    write(buf, '(A,I0)') "x = ", x
    if (trim(buf) /= "x = 42") error stop "formatted write failed"

    print *, "PASS"
end program

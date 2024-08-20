program subroutines_13
    character(20) :: cmd
    integer :: a
    integer :: b
    CALL get_command(cmd, a, b)

    print *, cmd
    print *, a
    print *, b
end program 
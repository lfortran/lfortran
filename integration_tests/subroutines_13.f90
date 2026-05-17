program subroutines_13
    character(20) :: cmd
    integer :: a
    integer :: b

    CALL get_command(cmd, a, b)
    print *, cmd
    print *, a
    print *, b

    CALL get_command(cmd, a)
    print *, cmd
    print *, a

    CALL get_command(cmd)
    print *, cmd

    CALL get_command()

    call get_command(length=a)
    print *, a

    call get_command(status=b)
    print *, b
end program 
program p
    integer :: a
    call get_command(length=a)
    print *, a

    call get_command(status=a)
    print *, a
end program 
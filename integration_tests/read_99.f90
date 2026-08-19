! Issue 12521: formatted '(a)' read of an empty record into a
! zero-length character variable must not raise EOF (glibc fgets(buf, 1)).
program read_99
    implicit none
    integer :: u
    character(len=:), allocatable :: line
    character(len=3) :: next

    open(newunit=u, status="scratch", form="formatted")
    write(u, '(a)') "foo"
    write(u, '(a)') ""
    write(u, '(a)') "bar"
    rewind(u)

    allocate(character(len=3) :: line)
    read(u, '(a)') line
    if (line /= "foo") error stop 1
    deallocate(line)

    allocate(character(len=0) :: line)
    read(u, '(a)') line
    if (len(line) /= 0) error stop 2
    deallocate(line)

    read(u, '(a)') next
    if (next /= "bar") error stop 3

    close(u)
end program read_99

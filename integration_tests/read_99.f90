! Issue 12521: formatted '(a)' read into a zero-length character variable
! must not raise EOF.  The internal-file case hits the same read_width==0
! path without depending on libc fgets(buf, 1) behaviour.
program read_99
    implicit none
    integer :: u
    character(len=:), allocatable :: line
    character(len=1) :: src

    src = "x"
    allocate(character(len=0) :: line)
    read(src, '(a)') line
    if (len(line) /= 0) error stop 1
    deallocate(line)

    open(newunit=u, file="read_99_data.txt", form="formatted", status="old")

    allocate(character(len=len("foo")) :: line)
    read(u, '(a)') line
    if (line /= "foo") error stop 2
    deallocate(line)

    allocate(character(len=0) :: line)
    read(u, '(a)') line
    if (len(line) /= 0) error stop 3
    deallocate(line)

    allocate(character(len=len("bar")) :: line)
    read(u, '(a)') line
    if (line /= "bar") error stop 4
    deallocate(line)

    close(u)
end program read_99

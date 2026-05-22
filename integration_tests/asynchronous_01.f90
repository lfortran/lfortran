program test_async
    implicit none
    integer :: u
    integer, asynchronous :: x

    open(newunit=u, file="test.dat", status="replace", &
         action="readwrite", form="unformatted", asynchronous="yes")
    write(u, asynchronous="yes") 42
    close(u)

    open(newunit=u, file="test.dat", status="old", &
         action="read", form="unformatted", asynchronous="yes")
    read(u, asynchronous="yes") x
    close(u, status="delete")

    if (x /= 42) error stop
    print *, x
end program test_async
! Module-qualified entities in input/output statements: output lists,
! input items, unit numbers, format strings and implied-DO loops.
module namespace_modules_18_io
    implicit none
    integer :: unit = 6
    integer :: vals(3) = [1, 2, 3]
    integer :: n = 0
    real :: r = 0
    character(len=*), parameter :: fmt = "(3i3)"
end module

program namespace_modules_18
    use, namespace :: io => namespace_modules_18_io
    implicit none
    character(len=40) :: buf
    integer :: i

    write(io%unit, io%fmt) io%vals
    write(buf, io%fmt) (io%vals(i), i = 1, 3)
    if (buf /= "  1  2  3") error stop

    buf = "42 2.5"
    read(buf, *) io%n, io%r
    if (io%n /= 42) error stop
    if (abs(io%r - 2.5) > 1e-6) error stop

    buf = "7 8 9"
    read(buf, *) (io%vals(i), i = 1, 3)
    if (any(io%vals /= [7, 8, 9])) error stop
    print *, io%n, io%r, io%vals
end program

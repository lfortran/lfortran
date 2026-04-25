program sizeof_01
    use iso_c_binding, only: c_int, c_size_t
    implicit none
    integer(c_int) :: x
    integer(c_size_t) :: s
    s = sizeof(x)
    print *, s
end program

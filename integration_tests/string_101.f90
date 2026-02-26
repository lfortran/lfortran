program string_101
    use iso_c_binding, only: c_int, c_ptr, c_f_pointer
    implicit none
    integer(c_int), pointer :: n
    integer(c_int), target :: val
    val = 5
    n => val
    block
        character(len=n) :: s
        s = "hello"
        if (len(s) /= 5) error stop
        if (s /= "hello") error stop
    end block
    print *, "ok"
end program

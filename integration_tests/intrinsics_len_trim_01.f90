program intrinsics_len_trim_01
    use iso_c_binding, only: c_int
    implicit none
    character(len=10) :: s
    integer(c_int) :: n

    s = "hello  "
    n = len_trim(s, kind=c_int)

    if (n /= 5_c_int) then
        print *, "Error: expected 5, got ", n
        error stop 1
    end if

    if (len_trim("world   ", kind=c_int) /= 5_c_int) then
        print *, "Error: expected 5, got ", len_trim("world   ", kind=c_int)
        error stop 2
    end if

    print *, "OK"
end program intrinsics_len_trim_01

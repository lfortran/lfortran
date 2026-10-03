program intrinsics_len_trim_01
    use iso_c_binding, only: c_int, c_long_long
    implicit none
    character(len=10) :: s
    integer(c_int) :: n
    integer(c_long_long) :: n8

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

    n8 = len_trim(s, kind=c_long_long)
    if (n8 /= 5_c_long_long) then
        print *, "Error: expected 5, got ", n8
        error stop 3
    end if

    print *, "OK"
end program intrinsics_len_trim_01

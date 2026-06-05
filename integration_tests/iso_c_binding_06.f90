program iso_c_binding_06
    use iso_c_binding, only: c_char, c_null_char, f_c_string
    implicit none

    character(kind=c_char, len=8) :: fstr
    character(kind=c_char, len=:), allocatable :: cstr

    fstr = "hello"

    cstr = f_c_string(fstr)
    if (len(cstr) /= 6) error stop 1
    if (cstr /= "hello" // c_null_char) error stop 2

    cstr = f_c_string(fstr, .true.)
    if (len(cstr) /= 9) error stop 3
    if (cstr(1:8) /= fstr) error stop 4
    if (cstr(9:9) /= c_null_char) error stop 5
end program

program bindc_12
    use, intrinsic :: iso_c_binding, only : c_char, c_int, c_null_char
    implicit none

    interface
        function c_chdir(path) bind(C, name="chdir") result(stat)
            import :: c_char, c_int
            character(kind=c_char, len=1), intent(in) :: path(*)
            integer(c_int) :: stat
        end function c_chdir
    end interface

    character(kind=c_char, len=1), allocatable :: cpath(:)
    integer(c_int) :: stat

    allocate(cpath(5))
    cpath = [ '/', 't', 'm', 'p', c_null_char ]

    if (cpath(5) /= c_null_char) then
        error stop "C string not NUL terminated"
    end if
    stat = c_chdir(cpath)
    if (stat /= 0) then
        error stop "chdir failed"
    end if

    print *, "OK: chdir(/tmp) succeeded"
end program bindc_12

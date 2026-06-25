program test_iargc_getarg
    ! Legacy F77 intrinsics iargc()/getarg(pos, value) should behave
    ! identically to the F2003 equivalents command_argument_count() and
    ! get_command_argument(number, value). See issue #10764.
    implicit none
    integer :: n1, n2, i
    character(len=128) :: arg_legacy, arg_modern

    n1 = iargc()
    n2 = command_argument_count()
    if (n1 /= n2) error stop "iargc() must equal command_argument_count()"

    call getarg(0, arg_legacy)
    call get_command_argument(0, arg_modern)
    if (arg_legacy /= arg_modern) then
        error stop "getarg(0) must match get_command_argument(0)"
    end if

    do i = 1, n1
        call getarg(i, arg_legacy)
        call get_command_argument(i, arg_modern)
        if (arg_legacy /= arg_modern) then
            error stop "getarg(i) must match get_command_argument(i)"
        end if
    end do

    print *, "iargc:", n1
end program

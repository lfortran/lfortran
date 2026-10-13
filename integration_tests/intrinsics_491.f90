! GET_COMMAND called from a C main: the Fortran runtime never sees the
! command line, so the command cannot be retrieved and STATUS must be a
! positive value (here 42, matching GET_COMMAND_ARGUMENT).
subroutine intrinsics_491_check() bind(c, name="intrinsics_491_check")
    implicit none
    integer :: n, st
    character(len=20) :: c
    call get_command(c, n, st)
    print *, n, st
    if (st <= 0) error stop
    if (n /= 0) error stop
    if (c /= "") error stop
    call get_command(status=st)
    if (st <= 0) error stop
    call get_command(length=n, status=st)
    if (st <= 0) error stop
    if (n /= 0) error stop
    call get_command_argument(0, c, n, st)
    if (st <= 0) error stop
end subroutine

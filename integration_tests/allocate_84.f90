! ERRMSG= of ALLOCATE and DEALLOCATE is assigned an explanatory message when
! an error occurs and is left unchanged otherwise (F2023 9.7.5).
program allocate_84
    implicit none
    real(8), allocatable :: a(:)
    integer(8) :: n
    integer :: ierr
    character(len=40) :: msg
    character(:), allocatable :: dmsg

    ! The request is far too large, so the allocation fails without
    ! committing any memory.
    n = huge(n) / 16

    ! Fixed-length ERRMSG=.
    msg = '#'
    allocate(a(n), stat=ierr, errmsg=msg)
    if (ierr <= 0) error stop 'out of memory: stat'
    if (allocated(a)) error stop 'out of memory: allocated'
    call check_assigned(msg, 'out of memory')

    msg = '#'
    allocate(a(3), stat=ierr, errmsg=msg)
    if (ierr /= 0) error stop 'success: stat'
    if (msg /= '#') error stop 'success: errmsg changed'

    allocate(a(3), stat=ierr, errmsg=msg)
    if (ierr <= 0) error stop 'already allocated: stat'
    call check_assigned(msg, 'already allocated')

    msg = '#'
    deallocate(a, stat=ierr, errmsg=msg)
    if (ierr /= 0) error stop 'deallocate success: stat'
    if (msg /= '#') error stop 'deallocate success: errmsg changed'
    deallocate(a, stat=ierr, errmsg=msg)
    if (ierr <= 0) error stop 'not allocated: stat'
    call check_assigned(msg, 'not allocated')

    ! Deferred-length ERRMSG=.
    dmsg = '#unchanged'
    allocate(a(n), stat=ierr, errmsg=dmsg)
    if (ierr <= 0) error stop 'deferred out of memory: stat'
    if (.not. allocated(dmsg)) error stop 'deferred out of memory: errmsg'
    call check_assigned(dmsg, 'deferred out of memory')

    dmsg = '#unchanged'
    allocate(a(3), stat=ierr, errmsg=dmsg)
    if (ierr /= 0) error stop 'deferred success: stat'
    if (dmsg /= '#unchanged') error stop 'deferred success: errmsg changed'
    allocate(a(3), stat=ierr, errmsg=dmsg)
    if (ierr <= 0) error stop 'deferred already allocated: stat'
    call check_assigned(dmsg, 'deferred already allocated')

    deallocate(a)
    dmsg = '#unchanged'
    deallocate(a, stat=ierr, errmsg=dmsg)
    if (ierr <= 0) error stop 'deferred not allocated: stat'
    call check_assigned(dmsg, 'deferred not allocated')

contains

    subroutine check_assigned(m, what)
        character(*), intent(in) :: m, what
        if (len_trim(m) == 0 .or. m(1:1) == '#') then
            print *, what, ': errmsg = "', m, '"'
            error stop 'errmsg not assigned'
        end if
    end subroutine

end program

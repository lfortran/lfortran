! Comprehensive test for INQUIRE PENDING and ASYNCHRONOUS keywords
! Tests various file access modes, unit states, and combinations
program inquire_13
    implicit none
    integer :: u, ios, unit_num, u1, u2, u3, i
    character(len=256) :: async_val, filename
    character(len=256) :: access_mode, form_val, named_val
    logical :: pending_val
    logical :: named_flag, opened_flag

    filename = "test_inquire_13.dat"

    ! ---- TEST 1: Stream access, opened file ----
    open(newunit=u, file=filename, status="replace", access="stream", &
         action="write", iostat=ios)
    if (ios /= 0) error stop

    write(u) 42
    rewind(u)

    async_val = "???"
    pending_val = .true.
    inquire(unit=u, asynchronous=async_val, pending=pending_val, iostat=ios)
    if (ios /= 0) error stop
    if (trim(async_val) /= "NO") then
        error stop
    end if
    if (pending_val) then
        error stop
    end if
    close(u)

    ! ---- TEST 2: Sequential access, formatted ----
    open(newunit=u, file=filename, status="old", access="sequential", &
         form="formatted", action="read", iostat=ios)
    if (ios /= 0) error stop

    async_val = "???"
    pending_val = .true.
    inquire(unit=u, asynchronous=async_val, pending=pending_val, iostat=ios)
    if (ios /= 0) error stop
    if (trim(async_val) /= "NO") error stop
    if (pending_val) error stop
    close(u)

    ! ---- TEST 3: Direct access, unformatted ----
    open(newunit=u, file=filename, status="old", access="direct", &
         recl=4, form="unformatted", action="read", iostat=ios)
    if (ios /= 0) error stop

    async_val = "???"
    pending_val = .true.
    inquire(unit=u, asynchronous=async_val, pending=pending_val, iostat=ios)
    if (ios /= 0) error stop
    if (trim(async_val) /= "NO") error stop
    if (pending_val) error stop
    close(u)

    ! ---- TEST 4: Multiple units with different access modes ----

    open(newunit=u1, file=filename, status="old", access="stream", &
         action="read", iostat=ios)
    if (ios /= 0) error stop

    open(newunit=u2, file=filename, status="old", access="sequential", &
         action="read", iostat=ios)
    if (ios /= 0) error stop

    open(newunit=u3, file=filename, status="old", access="direct", &
         recl=4, action="read", iostat=ios)
    if (ios /= 0) error stop

    ! Check u1 (stream)
    pending_val = .true.
    inquire(unit=u1, asynchronous=async_val, pending=pending_val, iostat=ios)
    if (ios /= 0 .or. trim(async_val) /= "NO" .or. pending_val) error stop

    ! Check u2 (sequential)
    pending_val = .true.
    inquire(unit=u2, asynchronous=async_val, pending=pending_val, iostat=ios)
    if (ios /= 0 .or. trim(async_val) /= "NO" .or. pending_val) error stop

    ! Check u3 (direct)
    pending_val = .true.
    inquire(unit=u3, asynchronous=async_val, pending=pending_val, iostat=ios)
    if (ios /= 0 .or. trim(async_val) /= "NO" .or. pending_val) error stop

    close(u1)
    close(u2)
    close(u3)

    ! ---- TEST 5: Default values for various scenarios ----
    open(newunit=u, file=filename, status="old", access="stream", iostat=ios)
    if (ios /= 0) error stop

    ! Initialize with non-default values to verify they're set
    async_val = "???"
    pending_val = .true.
    inquire(unit=u, asynchronous=async_val, pending=pending_val, iostat=ios)
    if (ios /= 0) error stop
    if (trim(async_val) /= "NO") error stop
    if (pending_val) error stop
    close(u)

    ! ---- TEST 6: File-based inquiry (not by unit) ----
    ! File-based inquire may return processor-dependent values for some keywords
    ! Just verify the call succeeds without error
    inquire(file=filename, asynchronous=async_val, iostat=ios)
    if (ios /= 0) error stop
    if (trim(async_val) /= "NO") error stop

    ! ---- TEST 7: Different file modes in sequence ----
    do i = 1, 3
        open(newunit=u, file=filename, status="old", access="stream", &
             action="read", iostat=ios)
        if (ios /= 0) error stop

        pending_val = .true.
        inquire(unit=u, asynchronous=async_val, pending=pending_val, iostat=ios)
        if (ios /= 0 .or. trim(async_val) /= "NO" .or. pending_val) error stop

        close(u)
    end do

    ! Clean up
    open(unit=10, file=filename, status='old', iostat=ios)
    if (ios == 0) close(10, status='delete')


end program inquire_13

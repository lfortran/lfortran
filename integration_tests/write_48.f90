module write_48_mod
    implicit none
    character(6) :: mbuf
contains
    subroutine fill_module_buffer(n)
        integer, intent(in) :: n
        write(mbuf, '(i0)') n
    end subroutine

    ! Each call writes into a local that was never assigned before
    logical function formats_as(n, expected) result(r)
        integer, intent(in) :: n
        character(*), intent(in) :: expected
        character(4) :: local
        write(local, '(i4)') n
        r = local == expected
    end function
end module

program write_48
    ! write: non-advancing output, iostat= on success and on an error,
    ! and internal files that were never assigned before the write
    use write_48_mod, only: mbuf, fill_module_buffer, formats_as
    implicit none
    character(10) :: never
    character(5) :: s
    integer :: ios, i
    real :: x

    ! Internal file never assigned before the write
    write(never, '(i3,"|")') 7
    if (never /= "  7|      ") error stop 1
    ! A second write reuses the same storage and blanks the rest
    write(never, '(a)') "ab"
    if (never /= "ab        ") error stop 2

    ! Module variable written from a procedure
    call fill_module_buffer(-12)
    if (mbuf /= "-12   ") error stop 3

    ! Repeated writes into the same variable
    do i = 1, 1000
        write(s, '(i5)') i
    end do
    if (s /= " 1000") error stop 4

    ! A local of a procedure, written on each call
    if (.not. formats_as(10, "  10")) error stop 5
    if (.not. formats_as(-305, "-305")) error stop 6

    ! iostat= on success and on an error
    ios = -99
    write(s, '(i3)', iostat=ios) 8
    if (ios /= 0) error stop 7
    if (s /= "  8  ") error stop 8

    ! The item does not match its edit descriptor
    x = 1.5
    ios = 0
    write(s, '(i5)', iostat=ios) x
    if (ios == 0) error stop 9

    ! Non-advancing output to a unit
    write(*, '(a)', advance='no') "ab"
    write(*, '(a)', advance='no') "cd"
    write(*, '(a)') "ef"
    ios = -1
    write(*, '(i0)', advance='no', iostat=ios) 42
    if (ios /= 0) error stop 10
    write(*, *)
end program

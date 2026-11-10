program case_11
    ! SELECT CASE with more than one range in a case selector (#5895)
    implicit none
    character(len=*), parameter :: input = 'aZm!@1 '
    character :: ch
    integer :: i, k, nvalid, nspecial, ninvalid, nint_ranges
    logical :: valid

    nvalid = 0
    nspecial = 0
    ninvalid = 0
    do i = 1, len(input)
        ch = input(i:i)
        valid = .false.
        select case (ch)
            case ('a':'z', 'A':'Z')
                valid = .true.
                nvalid = nvalid + 1
            case ('!', '@')
                nspecial = nspecial + 1
            case default
                ninvalid = ninvalid + 1
        end select
        if (valid .neqv. ((ch >= 'a' .and. ch <= 'z') .or. (ch >= 'A' .and. ch <= 'Z'))) error stop
    end do
    print *, nvalid, nspecial, ninvalid
    if (nvalid /= 3) error stop
    if (nspecial /= 2) error stop
    if (ninvalid /= 2) error stop

    nint_ranges = 0
    do k = -2, 12
        select case (k)
            case (:0, 3:4, 9:)
                nint_ranges = nint_ranges + 1
            case default
        end select
    end do
    ! -2..0 (3), 3..4 (2), 9..12 (4)
    if (nint_ranges /= 9) error stop
end program

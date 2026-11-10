program read_121
    ! Internal formatted read of a character variable inside SELECT CASE (#6230)
    implicit none
    character(20) :: key
    character(60) :: value
    character(256) :: br0file = 'default'
    integer :: nunknown
    nunknown = 0
    key = "nr"
    value = "133"
    select case (key)
    case ("br0file")
        read(value, '(A)') br0file
    case default
        write(*, *) "WARNING: Unknown key:", trim(key)
        nunknown = nunknown + 1
    end select
    if (nunknown /= 1) error stop
    if (br0file /= 'default') error stop

    key = "br0file"
    select case (key)
    case ("br0file")
        read(value, '(A)') br0file
    case default
        error stop
    end select
    print *, trim(br0file)
    if (br0file /= '133') error stop
    if (len_trim(br0file) /= 3) error stop
end program

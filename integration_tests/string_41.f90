program string_41
    print *, real_sp_to_string()
    if (real_sp_to_string() /= 'TestingLFortran') error stop
    contains
    pure function real_sp_to_string() result(string)
    character(len=:), allocatable :: string
    integer, parameter :: buffer_len = 128
    character(len=buffer_len) :: buffer
    buffer = 'TestingLFortran'
    string = trim(buffer)
    end function real_sp_to_string
end program

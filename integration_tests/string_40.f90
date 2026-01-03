program string_40
    print *, integer_i1_to_string(5)
    if (integer_i1_to_string(5) /= 'ingLFortran') error stop
    contains
    
    pure function integer_i1_to_string(pos) result(string)
    character(len=:), allocatable :: string
    
    integer, parameter :: buffer_len = 128
    character(len=buffer_len) :: buffer
    integer, intent(in) :: pos
    
    buffer = 'TestingLFortran'
    string = buffer(pos:)
    end function integer_i1_to_string
end program

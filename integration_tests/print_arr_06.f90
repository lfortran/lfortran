program print_arr_06

    character(5),allocatable:: arr_str(:)
    integer(8):: arr_int(3)
    real(8):: arr_real(4)
    logical :: arr_logical(3)
    integer(2) :: w
    real(4) ::m
    character(20) ::s
    character(200) :: FORMAT_STR 
    character(300) ::res
    character(300) ::lfortran_output
    character(300) ::gfortran_output
    allocate(arr_str(5))

    ! set format string
    FORMAT_STR = "(5a,2X,3i1,2X,4f3.1,2X,3l,2X,1i2,2X,1f5.3,2X,1a)"
    
    ! set values for scalar variables and arrays
    arr_str = ["a","b","c","d","e"]
    arr_int = [1,2,3]
    arr_real = [1.1,1.2,1.3,1.4]
    arr_logical = [.true.,.false.,.true.]
    w = 10
    m = 1.4
    s = "test"

    ! write to res with formatting 
    write(res,FORMAT_STR) arr_str,arr_int,arr_real,arr_logical,w,m,s
    
    ! print
    print FORMAT_STR,arr_str,arr_int,arr_real,arr_logical,w,m,s
    print "(a)",res

    ! Testing
    lfortran_output = "abcde  123  1.11.21.31.4  TFT  10  1.400  test"
    gfortran_output = "a    b    c    d    e      123  1.11.21.31.4  TFT  10  1.400  test"  

    ! GFortran and LFortran vary while printing array of characters.
    if(res /= lfortran_output .and. res /= gfortran_output) error stop 
end program print_arr_06
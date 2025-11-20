program format_37
implicit none
    character(len=100) :: output
    
    ! Test case 1: String and integer
    print "(*(g0,1x))", 'The answer is', 42
    write(output, '(*(g0,1x))') 'The answer is', 42
    print *, "Test 1: ", trim(output)
    if (trim(output) /= 'The answer is 42') error stop
    
    ! Test case 2: Multiple integers
    print "(*(g0,1x))", 43, 44, 45
    write(output, '(*(g0,1x))') 43, 44, 45
    print *, "Test 2: ", trim(output)
    if (trim(output) /= '43 44 45') error stop
    
    ! Test case 3: Mixed strings and integers pattern
    print "(*(g0,1x))", 'A', 'B', 10, 20, 'C', 'D', 30, 40
    write(output, '(*(g0,1x))') 'A', 'B', 10, 20, 'C', 'D', 30, 40
    print *, "Test 3: ", trim(output)
    if (trim(output) /= 'A B 10 20 C D 30 40') error stop
    
    print *, "All tests passed!"
    
end program format_37


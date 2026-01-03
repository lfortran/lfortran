program test_implied_do_loops_string

    
    implicit none
    integer :: i
    character(len=:), allocatable :: keywords(:)
    character(len=10) :: val
    character(len=5), allocatable :: fixed_str(:)
    
    keywords = [(val, i=1, 3)]
    
    if (size(keywords) /= 3) error stop "Test 1 Failed: size mismatch"
    
    fixed_str = [("test"//char(i+48), i=0, 2)]
    
    if (size(fixed_str) /= 3) error stop "Test 2 Failed: size mismatch"
    
    print *, "All tests passed!"
    
end program test_implied_do_loops_string

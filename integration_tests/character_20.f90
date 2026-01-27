program character_20
    implicit none
    character(len=10) :: long_str
    character(len=5) :: short_str
    logical :: result
    
    long_str = "hello12345"
    result = check_short(long_str)
    if (.not. result) error stop "Test 1 failed"
    
    short_str = "world"
    result = check_exact(short_str)
    if (.not. result) error stop "Test 2 failed"

    call test_with_allocatable()
    
contains
    function check_short(s) result(res)
        character(len=5), intent(in) :: s
        logical :: res
        res = (s == "hello")
    end function check_short
    
    function check_exact(s) result(res)
        character(len=5), intent(in) :: s
        logical :: res
        res = (s == "world")
    end function check_exact
    
    subroutine test_with_allocatable()
        character(len=8), allocatable :: alloc_str
        logical :: result
        
        allocate(alloc_str)
        alloc_str = "testing!"
        
        result = check_alloc(alloc_str)
        if (.not. result) error stop "Test 3 failed"
        
        deallocate(alloc_str)
    end subroutine test_with_allocatable
    
    function check_alloc(s) result(res)
        character(len=4), intent(in) :: s
        logical :: res
        res = (s == "test")
    end function check_alloc
    
end program character_20

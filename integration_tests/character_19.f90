program character_19
    implicit none
    character(len=4) :: test_char = "2021"
    character(len=:), allocatable :: alloc_char
    logical :: result
    
    ! Test 1: passing character(len=4) to function expecting character(len=1)
    ! Should use first character only
    result = isdigit(test_char)
    if (.not. result) error stop
    
    ! Test 2: passing allocatable character with runtime length
    ! Should also use first character only
    alloc_char = "1234"
    result = isdigit(alloc_char)
    if (.not. result) error stop
    
    ! Test 3: allocatable with different content
    alloc_char = "9999"
    result = isdigit(alloc_char)
    if (result) error stop
    
contains
    logical function isdigit(c)
        character(len=1), intent(in) :: c
        isdigit = index("12",c) > 0
    end function isdigit
end program character_19
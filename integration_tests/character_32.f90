program character_32
    implicit none

    type :: t
        character(len=:, kind=4), pointer :: s
    end type

    type(t) :: x
    character(kind=4) :: char4
    character(len=5, kind=4) :: str4
    character(len=:, kind=4), allocatable :: alloc_str4

    ! Test 1: Assignment of kind=4 to struct member section
    allocate(character(len=5, kind=4) :: x%s)
    x%s(4:4) = achar(int(z'0031'), kind=4)
    if (x%s(4:4) /= 4_"1") error stop 1
    
    ! Test 2: General character kind assignment
    char4 = 4_"A"
    if (char4 /= 4_"A") error stop 2

    ! Test 3: Allocatable character kind assignment
    allocate(character(len=3, kind=4) :: alloc_str4)
    alloc_str4 = 4_"ABC"
    alloc_str4(2:2) = 4_"X"
    if (alloc_str4 /= 4_"AXC") error stop 3

    ! Test 4: String concatenation and slicing with kind=4
    str4 = 4_"Hello"
    str4(1:2) = 4_"Hi"
    if (str4 /= 4_"Hillo") error stop 4

    deallocate(x%s)
    deallocate(alloc_str4)
    print *, "OK"
end program

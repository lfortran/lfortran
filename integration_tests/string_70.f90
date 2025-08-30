! Refer to ./doc/src/string_allocation.md

program string_70
    implicit none
    character(10), allocatable :: a
    character(:), allocatable :: b
    character(:), allocatable :: c

    !-----------------------------------------------!

    a = "HelloWorld"
    if(.not. allocated(a)) error stop
    a = b
    if(allocated(a)) error stop

    !-----------------------------------------------!

    b = "HelloWorld"
    if(.not. allocated(b) .or. len(b) /= 10 ) error stop
    b = c
    if(allocated(b)) error stop

end program
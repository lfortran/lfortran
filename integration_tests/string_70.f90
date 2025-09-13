! Refer to ./doc/src/string_allocation.md

program string_70
    implicit none
    character(:), allocatable :: STR

    allocate(character(0) :: STR)
    if(.not. allocated(STR)) error stop
    if(len(STR) /= 0) error stop

    deallocate(STR)
    
    if(allocated(STR)) error stop

end program
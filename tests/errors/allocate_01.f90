program allocate_01
    character(:), allocatable :: str
    allocate(character(len=10) :: str)
    
    deallocate(str)

    allocate(character(len=11) :: str)
    allocate(character(len=12) :: str) ! Error
end program 
program save_15
    implicit none
    call ss()
contains
    subroutine ss()
        integer, allocatable, save :: arr(:)
        allocate(arr(10))
    end subroutine ss
end program save_15

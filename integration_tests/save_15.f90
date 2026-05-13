program pointer_04
    implicit none
    call ss()
contains
    subroutine ss()
        integer, allocatable, save :: arr(:)
        allocate(arr(10))
    end subroutine ss
end program pointer_04

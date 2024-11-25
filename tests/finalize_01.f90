module finalize_01_mod
    contains
    subroutine test_01() ! Test that we deallocate allocatable characters
        character(:), allocatable :: str
        allocate(character(10) :: str)
    end subroutine test_01

    subroutine ss() ! Test deallocation on arrays.
        integer, allocatable :: arr(:)
        allocate(arr(10))
    end subroutine ss
end module finalize_01_mod


program finalize_01
    use finalize_01_mod
    character(:), allocatable :: str 
    integer, dimension(:), allocatable :: arr 
    allocate(character(10) :: str)
    call ss()
    call internal_sub()
    return ! Should insert deallocation for arr, str before this `return`.
    
    contains 

    subroutine internal_sub()
        character(:),allocatable :: str 
        real, allocatable :: arr_real(:,:)  
    end subroutine internal_sub
end program finalize_01
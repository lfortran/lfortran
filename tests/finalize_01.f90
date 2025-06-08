module finalize_01_mod
    contains
    subroutine test_01() ! Test that we deallocate allocatable characters
        character(:), allocatable :: str
        integer :: i
        allocate(character(10) :: str)
        return ! This will have `impliciteDeallocate` inserted before it.
        do i=1, 2
            return ! This won't have `impliciteDeallocate` inserted before it as we already have dead end. 
        end do
    end subroutine test_01

    subroutine ss() ! Test deallocation on arrays.
        integer, allocatable :: arr(:)
        allocate(arr(10))
        if(.true.) then
            return ! This will have `impliciteDeallocate` inserted before it.
        else 
            return ! This will have `impliciteDeallocate` inserted before it.
        end if
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
        bl : block
            integer, allocatable :: arr_in_block(:)
        exit bl ! This will have `impliciteDeallocate` inserted before it.
        return ! This won't have `impliciteDeallocate` inserted before it as we already have dead end. 
        end block bl 
        do while(.true.)
            return ! This will have `impliciteDeallocate` inserted before it.
        end do
    end subroutine internal_sub
end program finalize_01
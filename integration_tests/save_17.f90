module save_17_module
    implicit none
    integer, allocatable, save :: arr(:)
contains
    subroutine init_arr()
        if (.not. allocated(arr)) then
            allocate(arr(10))
        end if
    end subroutine init_arr
end module save_17_module

program save_17
    use save_17_module
    implicit none

    call init_arr()
end program save_17

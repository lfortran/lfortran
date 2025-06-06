module stdlib_io_npy_load_allocate_12
    implicit none
    integer, parameter :: dp = selected_real_kind(15)
contains
    subroutine load_npy_csp_4()
    complex, allocatable :: array(:,:,:,:)
    integer :: stat
    call allocator(array, stat)
    if (stat /= 9) error stop
    if (.not. allocated(array)) error stop
    contains
    subroutine allocator(array, stat)
        complex, allocatable, intent(out) :: array(:,:,:,:)
        integer, intent(out) :: stat
        allocate(array(1,2,3,4), stat=stat)
        if ( .not. allocated(array)) error stop
        stat = 9
    end subroutine allocator
    end subroutine load_npy_csp_4

    subroutine load_npy_cdp_4()
    contains
    subroutine allocator(array_cdp, stat)
        complex(dp), allocatable, intent(out) :: array_cdp(:,:,:,:)
        integer, intent(out) :: stat
    end subroutine allocator
    end subroutine load_npy_cdp_4

end module stdlib_io_npy_load_allocate_12

program allocate_12
    use stdlib_io_npy_load_allocate_12
    call load_npy_csp_4()
end program

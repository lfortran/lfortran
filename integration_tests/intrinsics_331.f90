subroutine shift_allocatable_array ()
    real(4), dimension(:), allocatable :: dc
    allocate (dc(2))
    dc = [1, 2]
    print *, cshift(dc,-1)
    deallocate(dc)
    return
end

program intrinsics_331
    implicit none
    real :: arr1(2) = [1., 2.]
    real :: arr2(2)
    call shift_elemements(2, arr1, arr2)

    print *, arr2
    if (any(arr2 /= [2., 1.])) error stop

    ! TODO: this causes segmentation fault currently
    ! call shift_allocatable_array()
    contains

    ! shifts the elements present in 'in_arr' by one index
    ! and outputs it in 'out_arr'
    subroutine shift_elemements(nc, in_arr, out_arr)
        implicit none
        integer, intent(in) :: nc
        real, dimension(nc), intent(in) :: in_arr
        real, dimension(nc), intent(out) :: out_arr
        out_arr = cshift(in_arr, shift=1)
    end
end program

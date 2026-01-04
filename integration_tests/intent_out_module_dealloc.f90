! Test that module subroutines properly deallocate intent(out) allocatable
! dummies at function entry per Fortran standard.
!
! This is an MRE for a bug where the IntentOutDeallocateVisitor pass
! incorrectly skipped ALL module procedures (thinking they were compiler-
! generated intrinsics) due to checking only deftype == Implementation.
module intent_out_module_dealloc_m
    implicit none
contains
    subroutine reset_array(x)
        integer, allocatable, intent(out) :: x(:)
        ! Per Fortran standard, x must be deallocated on entry
        if (allocated(x)) error stop 1
        allocate(x(3))
        x = [10, 20, 30]
    end subroutine reset_array
end module intent_out_module_dealloc_m

program intent_out_module_dealloc
    use intent_out_module_dealloc_m, only: reset_array
    implicit none

    integer, allocatable :: arr(:)

    ! First call: arr is not allocated
    call reset_array(arr)
    if (.not. allocated(arr)) error stop 2
    if (size(arr) /= 3) error stop 3
    if (arr(1) /= 10 .or. arr(2) /= 20 .or. arr(3) /= 30) error stop 4

    ! Second call: arr IS allocated, should be deallocated on entry
    deallocate(arr)
    allocate(arr(5))
    arr = [1, 2, 3, 4, 5]
    call reset_array(arr)
    if (.not. allocated(arr)) error stop 5
    if (size(arr) /= 3) error stop 6
    if (arr(1) /= 10 .or. arr(2) /= 20 .or. arr(3) /= 30) error stop 7

    print *, "PASS"
end program intent_out_module_dealloc

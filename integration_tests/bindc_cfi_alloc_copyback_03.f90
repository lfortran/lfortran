
program test_wrapper
    use iso_c_binding, only: c_int
    implicit none

    integer(c_int), allocatable :: a(:)

    call allocate_from_c(a)

    if (.not. allocated(a)) error stop "Allocation lost"
    if (size(a) /= 3) error stop "Wrong size"
    if (any(a /= [10, 20, 30])) error stop "Wrong values"

    deallocate(a)

    if (allocated(a)) error stop "Deallocation failed"


contains

    subroutine allocate_from_c(arr)
        use iso_c_binding, only: c_int
        implicit none

        integer(c_int), allocatable, intent(inout) :: arr(:)
        integer(c_int) :: ierr

        interface
            subroutine c_alloc(res, ierr) bind(c)
                import :: c_int
                integer(c_int), allocatable, intent(inout) :: res(:)
                integer(c_int), intent(out) :: ierr
            end subroutine
        end interface

        call c_alloc(arr, ierr)
	print *, "WRAPPER allocated:", allocated(arr)
if (allocated(arr)) print *, "WRAPPER size:", size(arr)
        if (ierr /= 0) error stop "C allocation failed"
    end subroutine

end program

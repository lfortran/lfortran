
program cfi_alloc
    use iso_c_binding, only: c_int
    implicit none
    interface
        subroutine c_alloc(res, ierr) bind(c)
            import :: c_int
            integer(c_int), allocatable, intent(inout) :: res(:)
            integer(c_int), intent(out) :: ierr
        end subroutine
    end interface
    integer(c_int), allocatable :: a(:)
    integer(c_int) :: ierr
    call c_alloc(a, ierr)
    print *, ierr, size(a), a(3)
    print *, a
	if (.not. allocated(a)) error stop "Allocation status incorrect"
	if (size(a) /= 3) error stop "Wrong array size"
	if (any(a /= [10, 20, 30])) error stop "Wrong values"

	deallocate(a)

	if (allocated(a)) error stop "Deallocation failed"

	print *, "All tests passed!"
end program


program test_real_alloc
    use iso_c_binding, only: c_int, c_double
    implicit none

    interface
        subroutine c_alloc_real(res, ierr) bind(c)
            import :: c_int, c_double
            real(c_double), allocatable, intent(inout) :: res(:)
            integer(c_int), intent(out) :: ierr
        end subroutine
    end interface

    real(c_double), allocatable :: a(:)
    integer(c_int) :: ierr

    call c_alloc_real(a, ierr)

    if (ierr /= 0) error stop "C allocation failed"
    if (.not. allocated(a)) error stop "Array not allocated"
    if (size(a) /= 3) error stop "Wrong size"

    if (any(a /= [1.5_c_double, 2.5_c_double, 3.5_c_double])) &
        error stop "Wrong values"

    deallocate(a)

    if (allocated(a)) error stop "Deallocation failed"

    print *, "Real array test passed!"
end program

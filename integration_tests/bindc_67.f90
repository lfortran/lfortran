program bindc_67
    use iso_c_binding
    implicit none
    interface
        subroutine bindc_67_alloc2d(res) bind(c)
            import :: c_double
            real(c_double), allocatable, intent(out) :: res(:,:)
        end subroutine
        subroutine bindc_67_dealloc(res) bind(c)
            import :: c_double
            real(c_double), allocatable, intent(inout) :: res(:,:)
        end subroutine
        subroutine bindc_67_ptr_alloc(res) bind(c)
            import :: c_int
            integer(c_int), pointer, intent(inout) :: res(:)
        end subroutine
        integer(c_int) function bindc_67_alloc_fn(res) bind(c)
            import :: c_int
            integer(c_int), allocatable, intent(inout) :: res(:)
        end function
        integer(c_int) function bindc_67_count(x) bind(c)
            import :: c_int
            integer(c_int), allocatable, intent(in) :: x(:)
        end function
    end interface
    real(c_double), allocatable :: a(:,:)
    integer(c_int), pointer :: p(:) => null()
    integer(c_int), allocatable :: b(:)
    integer(c_int) :: ierr

    call bindc_67_alloc2d(a)
    if (.not. allocated(a)) error stop 1
    if (any(lbound(a) /= [0, 1]) .or. any(ubound(a) /= [1, 3])) error stop 2
    if (a(1, 3) /= 6.0_c_double) error stop 3

    call bindc_67_dealloc(a)
    if (allocated(a)) error stop 4

    call bindc_67_ptr_alloc(p)
    if (.not. associated(p)) error stop 5
    if (lbound(p, 1) /= 5 .or. p(6) /= 8) error stop 6
    deallocate(p)

    ierr = bindc_67_alloc_fn(b)
    if (ierr /= 0 .or. .not. allocated(b)) error stop 7
    if (size(b) /= 4) error stop 8
    if (bindc_67_count(b) /= 4) error stop 9
end program

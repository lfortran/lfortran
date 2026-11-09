module bindc_cfi_alloc_copyback_06_mod
    use iso_c_binding, only: c_int
    implicit none
contains
    subroutine alloc_array(y) bind(c)
        integer(c_int), allocatable :: y(:)
        allocate(y(3))
        y = 15
    end subroutine

    subroutine alloc_scalar(y) bind(c)
        integer(c_int), allocatable :: y
        allocate(y)
        y = 16
    end subroutine
end module

program bindc_cfi_alloc_copyback_06
    use bindc_cfi_alloc_copyback_06_mod
    implicit none
    integer(c_int), allocatable :: a(:), s

    call alloc_array(a)
    if (.not. allocated(a)) error stop 1
    if (size(a) /= 3) error stop 2
    if (any(a /= 15)) error stop 3

    call alloc_scalar(s)
    if (.not. allocated(s)) error stop 4
    if (s /= 16) error stop 5
    print *, a, s
end program

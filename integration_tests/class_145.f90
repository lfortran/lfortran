module class_145_mod
    implicit none
    type :: mytype
        integer :: n
    contains
        procedure :: compute => mytype_compute
        procedure :: worker => mytype_worker
    end type
contains
    subroutine mytype_worker(me, x, res)
        class(mytype), intent(in) :: me
        real, intent(in) :: x(:)
        real, intent(out) :: res
        res = x(1) + real(me%n)
    end subroutine

    subroutine mytype_compute(me)
        class(mytype), intent(inout) :: me
        real, dimension(:), allocatable :: x
        real :: res
        allocate(x(3))
        x(1) = 1.0
        call me%worker(x, res)
        print *, res
        if (abs(res - 6.0) > 1e-6) error stop
    end subroutine
end module

program class_145
    use class_145_mod
    implicit none
    type(mytype) :: obj
    obj%n = 5
    call obj%compute()
    print *, "PASSED"
end program

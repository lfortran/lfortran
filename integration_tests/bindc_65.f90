module bindc_65_mod
    use iso_c_binding, only: c_int, c_double
    implicit none

    type, bind(c) :: bc_t
        integer(c_int) :: flag
        real(c_double) :: value
    end type

    type :: scalar_t
        type(bc_t), allocatable :: geometries(:)
    end type

    interface
        subroutine bindc_65_sum(n, geometries, total) bind(c)
            import :: c_int, c_double, bc_t
            integer(c_int), value, intent(in) :: n
            type(bc_t), intent(in) :: geometries(*)
            real(c_double), intent(out) :: total
        end subroutine
    end interface
end module

program bindc_65
    use bindc_65_mod
    implicit none
    type(scalar_t) :: sca
    real(c_double) :: total
    integer :: i

    allocate(sca%geometries(3))
    do i = 1, 3
        sca%geometries(i)%flag = i
        sca%geometries(i)%value = 1.5_c_double * i
    end do

    call bindc_65_sum(size(sca%geometries), sca%geometries, total)
    print *, total
    if (abs(total - 21.0_c_double) > 1e-12_c_double) error stop
end program

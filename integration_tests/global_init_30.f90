! A bind(c) procedure with an automatic array whose extent comes from module
! state and an internal procedure that reads that array by host
! association. Its entry dispatches the module's initialization before the
! extent is evaluated, as global_init_24 checks. The transformation that
! makes that possible must keep the internal procedure where Fortran allows
! one: CMakeLists.txt also compiles, with GFortran, the Fortran that the
! `fortran` backend prints for it, and an internal procedure cannot contain
! another.
module global_init_30_m
    use iso_c_binding, only: c_int
    implicit none
    type :: config
        integer :: count = 3
    end type
    ! Default initialization of an array's elements, which a loop of the
    ! module's initializer gives rather than static data.
    type(config) :: settings(2)
contains
    subroutine total(r) bind(c, name="global_init_30_total")
        integer(c_int), intent(out) :: r
        integer :: work(settings(1)%count)
        work = 2
        r = helper()
    contains
        integer function helper()
            helper = sum(work) + size(work)
        end function helper
    end subroutine total
end module global_init_30_m

program global_init_30
    use iso_c_binding, only: c_int
    use global_init_30_m
    implicit none
    integer(c_int) :: r
    call total(r)
    if (r /= 9) error stop 1
    settings(1)%count = 5
    call total(r)
    if (r /= 15) error stop 2
    print *, "ok"
end program global_init_30

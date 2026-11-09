! A generic whose only specific has an INTENT(IN) pointer dummy accepts a
! nonpointer target, including an allocatable with the TARGET attribute and
! an array section of either.
module generic_interface_distinguishable_06_m
    implicit none
    interface g_only_ptr
        module procedure ptr_in
    end interface
contains
    subroutine ptr_in(x)
        real, pointer, intent(in) :: x(:)
        if (.not. associated(x)) error stop 1
        if (size(x) < 1) error stop 2
        if (abs(x(1) - 4.0) > 1.0e-6) error stop 3
    end subroutine
end module

program generic_interface_distinguishable_06
    use generic_interface_distinguishable_06_m
    implicit none
    real, target :: t(2)
    real, allocatable, target :: xt(:)
    t = 4.0
    allocate(xt(2))
    xt = 4.0
    call g_only_ptr(t)
    call g_only_ptr(t(1:2))
    call g_only_ptr(xt)
    call g_only_ptr(xt(1:2))
    print *, "ok"
end program

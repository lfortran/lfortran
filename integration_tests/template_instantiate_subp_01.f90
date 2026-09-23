! Tests R1626: instantiating a templated subprogram under a local name with
! `instantiate :: local-name => templated-subp-name {args}`.
module template_instantiate_subp_01_m
    implicit none

contains

    template subroutine swap{t}(x, y)
        deferred type :: t
        type(t), intent(inout) :: x, y
        type(t) :: tmp
        tmp = x
        x = y
        y = tmp
    end subroutine

    subroutine test_int()
        instantiate :: swap_int => swap {integer}
        integer :: a, b
        a = 1
        b = 2
        call swap_int(a, b)
        if (a /= 2) error stop
        if (b /= 1) error stop
    end subroutine

    subroutine test_real()
        instantiate :: swap_real => swap {real}
        real :: a, b
        a = 1.5
        b = 2.5
        call swap_real(a, b)
        if (abs(a - 2.5) > 1e-6) error stop
        if (abs(b - 1.5) > 1e-6) error stop
    end subroutine

end module

program template_instantiate_subp_01
    use template_instantiate_subp_01_m
    implicit none
    call test_int()
    call test_real()
end program

! Tests R1625 with a rename-list and no ONLY: `instantiate tmpl {args}, b => a`
! makes every public entity of the instantiation accessible and additionally
! gives `a` the local name `b`.
module template_instantiate_rename_01_m
    implicit none

    template tmpl_t(t)
        deferred type :: t
    contains
        function id(x) result(r)
            type(t), intent(in) :: x
            type(t) :: r
            r = x
        end function

        function second(x, y) result(r)
            type(t), intent(in) :: x, y
            type(t) :: r
            r = x
            r = y
        end function
    end template

contains

    subroutine test_rename()
        instantiate tmpl_t {integer}, id_int => id
        integer :: a
        a = 5
        ! the renamed entity is accessible under its local name ...
        if (id_int(a) /= 5) error stop
        ! ... and, unlike the ONLY form, so is every other entity, under its
        ! own name
        if (id(a) /= 5) error stop
        if (second(a, 7) /= 7) error stop
    end subroutine

end module

program template_instantiate_rename_01
    use template_instantiate_rename_01_m
    implicit none
    call test_rename()
end program

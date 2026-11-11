module procedure_pointer_51_mod
    type :: t
        integer :: n = 0
        procedure(iface), pointer :: f => null()
    end type
    abstract interface
        subroutine iface(me, x)
            import :: t
            class(t), intent(inout) :: me
            real, dimension(me%n), intent(inout) :: x
        end subroutine
    end interface
contains
    subroutine set_one(me, x)
        class(t), intent(inout) :: me
        real, dimension(me%n), intent(inout) :: x
        x = 1.0
    end subroutine
end module

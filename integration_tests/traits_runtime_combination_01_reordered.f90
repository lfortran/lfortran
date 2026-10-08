module traits_runtime_combination_01_reordered_m
    use traits_runtime_combination_01_contracts_m, only: B => ILabel, A => IValue, IRich
    implicit none
contains
    subroutine slot_in(object, live, n)
        class(B + A), allocatable, intent(in) :: object
        logical, intent(in) :: live
        integer, intent(in) :: n
        if (allocated(object) .neqv. live) error stop 1310
        if (live) then
            if (object%value() /= n) error stop 1311
        end if
    end subroutine
    subroutine slot_inout(object, source)
        class(B + A + B), allocatable, intent(inout) :: object
        class(IRich), intent(in) :: source
        object = source
    end subroutine
    subroutine slot_out(object, source)
        class(B + A), allocatable, intent(out) :: object
        class(IRich), intent(in) :: source
        if (allocated(object)) error stop 1312
        allocate(object, source=source)
    end subroutine
    subroutine slot_unspecified(object, source)
        class(B + A), allocatable :: object
        class(IRich), intent(in) :: source
        if (allocated(object)) deallocate(object)
        allocate(object, source=source)
    end subroutine
    subroutine pointer_out(object, source)
        class(B + A), pointer, intent(out) :: object
        class(IRich), pointer, intent(in) :: source
        class(A + B), pointer :: local
        local => source
        object => local
        nullify(local)
    end subroutine
    subroutine pointer_inout(object, source)
        class(B + A + A), pointer, intent(inout) :: object
        class(IRich), pointer, intent(in) :: source
        object => source
    end subroutine
    subroutine pointer_unspecified(object, source)
        class(B + A), pointer :: object
        class(IRich), pointer, intent(in) :: source
        object => source
    end subroutine
end module

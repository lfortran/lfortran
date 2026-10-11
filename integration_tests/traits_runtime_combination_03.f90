module traits_runtime_combination_03_first
    implicit none
    abstract interface :: I
        integer function value()
        end function
    end interface
    abstract interface :: IEmpty
    end interface
end module

module traits_runtime_combination_03_second
    implicit none
    abstract interface :: I
        integer function value()
        end function
    end interface
    abstract interface :: IEmpty
    end interface
end module

module traits_runtime_combination_03_payload
    implicit none
    type :: Box
        integer :: n = 0
    end type
contains
    integer function get_value(self)
        type(Box), intent(in) :: self
        get_value = self%n
    end function
end module

module traits_runtime_combination_03_impl_a
    use traits_runtime_combination_03_first, only: I, IEmpty
    use traits_runtime_combination_03_payload, only: Box, get_value
    implicit none
    implements I + IEmpty :: Box
        procedure, pass :: value => get_value
    end implements
end module

module traits_runtime_combination_03_impl_b
    use traits_runtime_combination_03_second, only: I, IEmpty
    use traits_runtime_combination_03_payload, only: Box, get_value
    implicit none
    implements I + IEmpty :: Box
        procedure, pass :: value => get_value
    end implements
end module

module traits_runtime_combination_03_order_a
    use traits_runtime_combination_03_impl_a, A => I, EA => IEmpty
    use traits_runtime_combination_03_impl_b, B => I, EB => IEmpty
    use traits_runtime_combination_03_payload, only: Box
    implicit none
contains
    subroutine associate_a(object, view, empty)
        type(Box), target, intent(in) :: object
        class(A + B), pointer, intent(out) :: view
        class(EA + EB), pointer, intent(out) :: empty
        view => object
        empty => object
    end subroutine
    integer function observe_a(object)
        class(A + B), intent(in) :: object
        observe_a = object%value()
    end function
end module

module traits_runtime_combination_03_order_b
    use traits_runtime_combination_03_impl_b, B => I, EB => IEmpty
    use traits_runtime_combination_03_impl_a, A => I, EA => IEmpty
    use traits_runtime_combination_03_payload, only: Box
    implicit none
contains
    subroutine associate_b(object, view)
        type(Box), target, intent(in) :: object
        class(B + A + B), pointer, intent(inout) :: view
        view => object
    end subroutine
    subroutine own_empty(object, view)
        type(Box), intent(in) :: object
        class(EB + EA), allocatable, intent(out) :: view
        allocate(view, source=object)
    end subroutine
    integer function observe_b(object)
        class(B + A), intent(in) :: object
        observe_b = object%value()
    end function
end module

program traits_runtime_combination_03
    use traits_runtime_combination_03_first, only: A => I, EA => IEmpty
    use traits_runtime_combination_03_second, only: B => I, EB => IEmpty
    use traits_runtime_combination_03_payload, only: Box
    use traits_runtime_combination_03_order_a, only: associate_a, observe_a
    use traits_runtime_combination_03_order_b, only: associate_b, observe_b, own_empty
    implicit none
    type(Box), target :: object, other
    class(B + A), pointer :: view => null(), alias => null()
    class(A), pointer :: first => null()
    class(B), pointer :: second => null()
    class(EA + EB), pointer :: empty => null(), empty_alias => null()
    class(EA), pointer :: member => null()
    class(EA + EB), allocatable, target :: owner

    object%n = 17
    other%n = 29
    call associate_a(object, view, empty)
    alias => view
    first => view
    second => view
    if (observe_a(view) /= 17 .or. observe_b(view) /= 17) error stop 1501
    if (first%value() /= 17 .or. second%value() /= 17) error stop 1502
    if (.not. associated(first, view) .or. .not. associated(second, view)) error stop 1503
    call associate_b(other, view)
    if (view%value() /= 29 .or. alias%value() /= 17) error stop 1504
    if (associated(view, alias)) error stop 1505
    member => empty
    empty_alias => empty
    if (.not. associated(member, empty)) error stop 1506
    nullify(empty)
    if (.not. associated(empty_alias)) error stop 1507
    call own_empty(object, owner)
    member => owner
    if (.not. allocated(owner) .or. .not. associated(member, owner)) error stop 1508
    if (associated(member, empty_alias)) error stop 1509
    nullify(member, empty_alias, view, alias, first, second)
    deallocate(owner)
end program

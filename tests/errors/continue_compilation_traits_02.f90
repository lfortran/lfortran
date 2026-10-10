module traits_component_body_error_types
    abstract interface :: IValue
        pure integer function value()
        end function
    end interface
    type :: Seed
        integer :: n = 1
    end type
    implements IValue :: Seed
        procedure :: value => seed_value
    end implements
    type :: Holder
        class(IValue), allocatable :: item
    end type
contains
    pure integer function seed_value(self)
        type(Seed), intent(in) :: self
        seed_value = self%n
    end function
    subroutine replace(item)
        class(IValue), allocatable, intent(inout) :: item
        if (allocated(item)) deallocate(item)
    end subroutine
end module

module traits_component_body_error_readonly
    use traits_component_body_error_types
contains
    subroutine clear(object)
        type(Holder), intent(in) :: object
        deallocate(object%item)
    end subroutine
    subroutine assign(object, source)
        type(Holder), intent(in) :: object
        type(Seed), intent(in) :: source
        object%item = source
    end subroutine
    subroutine pass(object)
        type(Holder), intent(in) :: object
        call replace(object%item)
    end subroutine
end module

module traits_component_body_error_pure_pointer
    use traits_component_body_error_types
contains
    pure subroutine overwrite(x, y)
        type(Holder), pointer, intent(inout) :: x
        type(Holder), intent(in) :: y
        x = y
    end subroutine
    pure subroutine dispose(x)
        type(Holder), pointer, intent(inout) :: x
        deallocate(x)
    end subroutine
end module

module traits_component_body_error_pure_indirect
    use traits_component_body_error_types
contains
    subroutine overwrite(x, y)
        type(Holder), intent(inout) :: x
        type(Holder), intent(in) :: y
        x = y
    end subroutine
    subroutine dispose(x)
        type(Holder), pointer, intent(inout) :: x
        deallocate(x)
    end subroutine
    pure subroutine assign_through(x, y)
        type(Holder), intent(inout) :: x
        type(Holder), intent(in) :: y
        call overwrite(x, y)
    end subroutine
    pure subroutine dispose_through(x)
        type(Holder), pointer, intent(inout) :: x
        call dispose(x)
    end subroutine
end module

module traits_component_body_error_constructor
    use traits_component_body_error_types
contains
    subroutine build()
        type(Holder) :: x
        type(Seed) :: s
        x = Holder(s)
    end subroutine
end module

module traits_component_body_error_move_alloc
    use traits_component_body_error_types
contains
    subroutine move(x, y)
        type(Holder), intent(inout) :: x, y
        call move_alloc(x%item, y%item)
    end subroutine
end module

module traits_component_body_error_pure_generic
    use traits_component_body_error_types
    abstract interface :: ITagged
        integer function tag()
        end function
    end interface
    implements ITagged :: Holder
        procedure :: tag => holder_tag
    end implements
contains
    integer function holder_tag(self)
        type(Holder), intent(in) :: self
        holder_tag = 0
    end function
    pure subroutine swap{ITagged :: T}(x, y)
        type(T), intent(inout) :: x, y
        type(T) :: saved
        saved = x
        x = y
        y = saved
    end subroutine
    subroutine use_swap(a, b)
        type(Holder), intent(inout) :: a, b
        call swap(a, b)
    end subroutine
end module

module traits_component_body_error_pure_template
    use traits_component_body_error_types
    requirement copyable {t}
        deferred type :: t
    end requirement
    template exchange_tmpl {t}
        require :: copyable {t}
        private
        public :: exchange
    contains
        pure subroutine exchange(x, y)
            type(t), intent(inout) :: x, y
            type(t) :: saved
            saved = x
            x = y
            y = saved
        end subroutine
    end template
contains
    subroutine use_exchange(a, b)
        instantiate exchange_tmpl {Holder}, only: exchange_holder => exchange
        type(Holder), intent(inout) :: a, b
        call exchange_holder(a, b)
    end subroutine
end module

module traits_component_body_error_pure_forward
    use traits_component_body_error_types
    abstract interface
        subroutine copy_holder(x, y)
            import :: Holder
            type(Holder), intent(inout) :: x
            type(Holder), intent(in) :: y
        end subroutine
    end interface
contains
    pure subroutine assign_later(x, y)
        type(Holder), intent(inout) :: x
        type(Holder), intent(in) :: y
        call overwrite(x, y)
    end subroutine
    pure subroutine assign_twice_removed(x, y)
        type(Holder), intent(inout) :: x
        type(Holder), intent(in) :: y
        call relay(x, y)
    end subroutine
    pure subroutine assign_recursively(x, y)
        type(Holder), intent(inout) :: x
        type(Holder), intent(in) :: y
        call ping(x, y, 2)
    end subroutine
    pure subroutine assign_through_dummy(x, y)
        type(Holder), intent(inout) :: x
        type(Holder), intent(in) :: y
        call apply(overwrite, x, y)
    end subroutine
    pure subroutine move_holders(x, y)
        type(Holder), allocatable, intent(inout) :: x(:), y(:)
        call move_alloc(x, y)
    end subroutine
    pure integer function count_reshaped(x)
        type(Holder), intent(in) :: x(:)
        count_reshaped = size(reshape(x, [size(x)]))
    end function
    subroutine relay(x, y)
        type(Holder), intent(inout) :: x
        type(Holder), intent(in) :: y
        call overwrite(x, y)
    end subroutine
    recursive subroutine ping(x, y, n)
        type(Holder), intent(inout) :: x
        type(Holder), intent(in) :: y
        integer, intent(in) :: n
        if (n > 0) call pong(x, y, n - 1)
    end subroutine
    recursive subroutine pong(x, y, n)
        type(Holder), intent(inout) :: x
        type(Holder), intent(in) :: y
        integer, intent(in) :: n
        if (n == 0) then
            x = y
        else
            call ping(x, y, n)
        end if
    end subroutine
    subroutine apply(f, x, y)
        procedure(copy_holder) :: f
        type(Holder), intent(inout) :: x
        type(Holder), intent(in) :: y
        call f(x, y)
    end subroutine
    subroutine overwrite(x, y)
        type(Holder), intent(inout) :: x
        type(Holder), intent(in) :: y
        x = y
    end subroutine
end module

module traits_component_body_error_pure_relay
    use traits_component_body_error_types
    type :: Box
        type(Holder) :: h
    contains
        procedure :: assign_box
        generic :: assignment(=) => assign_box
    end type
    type :: Labeled
        type(Holder) :: h
        integer :: tag = 0
    contains
        procedure :: assign_label
        generic :: assignment(=) => assign_label
    end type
    type :: Copier
        integer :: k = 0
    contains
        procedure :: copy_into
    end type
contains
    pure subroutine copy_box(x, y)
        type(Box), intent(inout) :: x
        type(Box), intent(in) :: y
        x = y
    end subroutine
    pure subroutine copy_label(x, y)
        type(Labeled), intent(inout) :: x
        type(Labeled), intent(in) :: y
        x = y
    end subroutine
    pure subroutine copy_bound(c, x, y)
        type(Copier), intent(in) :: c
        type(Holder), intent(inout) :: x
        type(Holder), intent(in) :: y
        call c%copy_into(x, y)
    end subroutine
    subroutine assign_box(lhs, rhs)
        class(Box), intent(inout) :: lhs
        class(Box), intent(in) :: rhs
        lhs%h = rhs%h
    end subroutine
    subroutine assign_label(lhs, rhs)
        class(Labeled), intent(inout) :: lhs
        class(Labeled), intent(in) :: rhs
        lhs%tag = rhs%tag
    end subroutine
    subroutine copy_into(self, x, y)
        class(Copier), intent(in) :: self
        type(Holder), intent(inout) :: x
        type(Holder), intent(in) :: y
        x = y
    end subroutine
end module

module traits_component_body_error_pure_generic_forward
    use traits_component_body_error_types
    abstract interface :: IHeld
        integer function held()
        end function
    end interface
    implements IHeld :: Holder
        procedure :: held => holder_held
    end implements
contains
    pure subroutine copy_generic(x, y)
        type(Holder), intent(inout) :: x
        type(Holder), intent(in) :: y
        call copy(x, y)
    end subroutine
    integer function holder_held(self)
        type(Holder), intent(in) :: self
        holder_held = 0
    end function
    subroutine copy{IHeld :: T}(x, y)
        type(T), intent(inout) :: x
        type(T), intent(in) :: y
        x = y
    end subroutine
end module

program traits_component_body_error_pure_contained
    use traits_component_body_error_types
    type(Holder) :: p, q
    call assign_contained(p, q)
contains
    pure subroutine assign_contained(x, y)
        type(Holder), intent(inout) :: x
        type(Holder), intent(in) :: y
        call overwrite_contained(x, y)
    end subroutine
    subroutine overwrite_contained(x, y)
        type(Holder), intent(inout) :: x
        type(Holder), intent(in) :: y
        x = y
    end subroutine
end program

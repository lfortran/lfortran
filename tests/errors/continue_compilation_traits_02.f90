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

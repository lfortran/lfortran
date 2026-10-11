module traits_runtime_component_13_m
    implicit none
    integer :: finals = 0, final_sum = 0
    abstract interface :: IValue
        pure integer function value()
        end function
    end interface
    abstract interface :: ITagged
        integer function tag()
        end function
    end interface
    type :: Payload
        integer :: n = 0
    contains
        final :: finish_payload
    end type
    implements IValue :: Payload
        procedure :: value => payload_value
    end implements
    type :: Holder
        class(IValue), allocatable :: item
    end type
    implements ITagged :: Holder
        procedure :: tag => holder_tag
    end implements
    type, extends(Holder) :: Child
        integer :: extra = 0
    end type
contains
    pure integer function payload_value(self)
        type(Payload), intent(in) :: self
        payload_value = self%n
    end function
    subroutine finish_payload(self)
        type(Payload), intent(inout) :: self
        finals = finals + 1
        final_sum = final_sum + self%n
        self%n = -900
    end subroutine
    integer function holder_tag(self)
        type(Holder), intent(in) :: self
        holder_tag = 0
        if (allocated(self%item)) holder_tag = self%item%value()
    end function
    subroutine swap{ITagged :: T}(x, y)
        type(T), intent(inout) :: x, y
        type(T) :: saved
        saved = x
        x = y
        y = saved
    end subroutine
    subroutine check(ok, code)
        logical, intent(in) :: ok
        integer, intent(in) :: code
        if (.not. ok) error stop code
    end subroutine
end module

module traits_runtime_component_13_template_m
    implicit none
    requirement copyable {t}
        deferred type :: t
    end requirement
    template exchange_tmpl {t}
        require :: copyable {t}
        private
        public :: exchange
    contains
        subroutine exchange(x, y)
            type(t), intent(inout) :: x, y
            type(t) :: saved
            saved = x
            x = y
            y = saved
        end subroutine
    end template
end module

program traits_runtime_component_13
    use traits_runtime_component_13_m
    use traits_runtime_component_13_template_m
    implicit none
    instantiate exchange_tmpl {Holder}, only: exchange_holder => exchange
    type(Holder) :: a, b
    class(Holder), allocatable :: q
    class(*), allocatable :: u
    type(Payload) :: seed

    seed%n = 1
    a%item = seed
    seed%n = 2
    b%item = seed
    ! The saved local is released when each instance returns.
    call swap(a, b)
    call check(a%item%value() == 2 .and. b%item%value() == 1, 1)
    call check(finals == 3 .and. final_sum == 4, 2)
    call exchange_holder(a, b)
    call check(a%item%value() == 1 .and. b%item%value() == 2, 3)
    call check(finals == 6 .and. final_sum == 9, 4)

    allocate(Child :: q)
    seed%n = 6
    select type (r => q)
    class is (Child)
        r%item = seed
    end select
    select type (q)
    type is (Child)
        call check(q%item%value() == 6, 5)
    class default
        error stop 6
    end select
    u = q
    select type (u)
    type is (Child)
        call check(u%item%value() == 6, 7)
        deallocate(u%item)
    class default
        error stop 8
    end select
    call check(finals == 7 .and. final_sum == 15, 9)
    deallocate(q, u, a%item, b%item)
    call check(finals == 10 .and. final_sum == 24, 10)
end program

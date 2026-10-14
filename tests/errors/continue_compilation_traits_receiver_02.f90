! Explicit trait receivers (TD-36): calls. An intent(inout) or intent(out)
! receiver needs a definable object, whatever path dispatches the call; an
! intent(in) receiver, or one without INTENT, accepts any object. The
! receiver is not an argument of the call. PURE messages follow the ordinary
! PURE rules. Each procedure or module whose name starts with bad_ has one
! error; procedures whose names start with ok_ are valid controls, and
! ordinary_ procedures are the same errors without traits.
module cc_traits_receiver_calls_contracts
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none
    abstract interface :: ICounter
        integer function value(self)
            type(itself), intent(in), pass :: self
        end function
        subroutine bump(self, by)
            type(itself), intent(inout), pass :: self
            integer, intent(in) :: by
        end subroutine
        subroutine reset(seed, self)
            integer, intent(in) :: seed
            type(itself), intent(out), pass :: self
        end subroutine
        subroutine touch(self)
            type(itself), pass :: self
        end subroutine
    end interface
    abstract interface :: IMember
        integer | real(real64)
    end interface
    abstract interface :: IMeter
        integer function record{IMember :: T}(self, x)
            type(itself), intent(inout), pass :: self
            type(T), intent(in) :: x(:)
        end function
    end interface
    abstract interface :: INumeric
        initial(n)
            integer, intent(in) :: n
        end initial
        function operator(+)(lhs, rhs) result(res)
            type(itself), intent(in) :: lhs, rhs
            type(itself) :: res
        end function
    end interface
    abstract interface :: IAccumulator
        integer function restart{INumeric :: T}(self, x)
            type(itself), intent(out), pass :: self
            type(T), intent(in) :: x(:)
        end function
    end interface
    abstract interface :: IStep
        pure integer function peek(self)
            type(itself), intent(in), pass :: self
        end function
        pure subroutine step(self)
            type(itself), intent(inout), pass :: self
        end subroutine
    end interface
end module cc_traits_receiver_calls_contracts

module cc_traits_receiver_calls_providers
    use cc_traits_receiver_calls_contracts
    implicit none
    implements INumeric :: integer
    end implements integer
    type :: Counter
        integer :: n = 0
    end type
    implements ICounter :: Counter
        procedure, pass :: value => counter_value
        procedure, pass :: bump => counter_bump
        procedure, pass(self) :: reset => counter_reset
        procedure, pass :: touch => counter_touch
    end implements
    type :: Holder
        type(Counter) :: c
    end type
    type(Counter), parameter :: fixed = Counter(1)
    type, sealed :: Meter
        integer :: hits = 0
    end type
    implements IMeter :: Meter
        procedure, pass :: record => meter_record
    end implements
    type, sealed :: Acc
        integer :: n = 0
    end type
    implements IAccumulator :: Acc
        procedure, pass :: restart => acc_restart
    end implements
    type, sealed :: Steps
        integer :: n = 0
    end type
    implements IStep :: Steps
        procedure, pass :: peek => steps_peek
        procedure, pass :: step => steps_step
    end implements
contains
    integer function counter_value(self)
        class(Counter), intent(in) :: self
        counter_value = self%n
    end function
    subroutine counter_bump(self, by)
        class(Counter), intent(inout) :: self
        integer, intent(in) :: by
        self%n = self%n + by
    end subroutine
    subroutine counter_reset(self, seed)
        class(Counter), intent(out) :: self
        integer, intent(in) :: seed
        self%n = seed
    end subroutine
    subroutine counter_touch(self)
        class(Counter) :: self
        print *, self%n
    end subroutine
    type(Counter) function make_counter()
        make_counter%n = 2
    end function
    integer function meter_record{IMember :: T}(self, x) result(r)
        type(Meter), intent(inout) :: self
        type(T), intent(in) :: x(:)
        self%hits = self%hits + size(x)
        r = self%hits
    end function
    integer function acc_restart{INumeric :: T}(self, x) result(r)
        type(Acc), intent(out) :: self
        type(T), intent(in) :: x(:)
        self%n = size(x)
        r = self%n
    end function
    pure integer function steps_peek(self)
        type(Steps), intent(in) :: self
        steps_peek = self%n
    end function
    pure subroutine steps_step(self)
        type(Steps), intent(inout) :: self
        self%n = self%n + 1
    end subroutine
end module cc_traits_receiver_calls_providers

module cc_traits_receiver_calls
    use cc_traits_receiver_calls_contracts
    use cc_traits_receiver_calls_providers
    implicit none
contains
    ! C01: a read-only view cannot be reset.
    subroutine bad_reset_readonly_view(v)
        class(ICounter), intent(in) :: v
        call v%reset(1)
    end subroutine
    ! C02: nor can a read-only owner be mutated.
    subroutine bad_bump_readonly_owner(o)
        class(ICounter), allocatable, intent(in) :: o
        call o%bump(1)
    end subroutine
    ! C03: a named constant is not definable.
    subroutine bad_reset_constant()
        call fixed%reset(2)
    end subroutine
    ! C04: nor is a component of an intent(in) dummy.
    subroutine bad_reset_readonly_component(h)
        type(Holder), intent(in) :: h
        call h%c%reset(3)
    end subroutine
    ! C05: nor an associate name of a read-only view.
    subroutine bad_bump_associate_view(v)
        class(ICounter), intent(in) :: v
        associate (r => v)
            call r%bump(1)
        end associate
    end subroutine
    ! C06: nor an associate name of a function result.
    subroutine bad_bump_associate_result()
        associate (r => make_counter())
            call r%bump(1)
        end associate
    end subroutine
    ! C07: nor an associate name of a parenthesized variable.
    subroutine bad_reset_associate_parentheses(c)
        type(Counter), intent(inout) :: c
        associate (r => (c))
            call r%reset(4)
        end associate
    end subroutine
    ! C08: nor a SELECT TYPE name of a read-only view.
    subroutine bad_bump_select_type(v)
        class(ICounter), intent(in) :: v
        select type (s => v)
        class default
            call s%bump(1)
        end select
    end subroutine
    ! C09: a generic body cannot reset an intent(in) dummy.
    subroutine bad_reset_generic{ICounter :: T}(x)
        type(T), intent(in) :: x
        call x%reset(5)
    end subroutine
    ! C10: a generic message over a closed type set checks its receiver.
    integer function bad_record_readonly(v)
        class(IMeter), intent(in) :: v
        bad_record_readonly = v%record([1, 2])
    end function
    ! C11: so does a generic message over an open protocol.
    integer function bad_restart_readonly(a)
        class(IAccumulator), intent(in) :: a
        bad_restart_readonly = a%restart([1, 2])
    end function
    ! C12: the receiver is not an argument, even by keyword.
    subroutine bad_receiver_keyword(v)
        class(ICounter), intent(inout) :: v
        call v%bump(self=v, by=1)
    end subroutine
    ! C13: a PURE procedure cannot call an impure message.
    pure integer function bad_pure_calls_impure(v)
        class(ICounter), intent(in) :: v
        bad_pure_calls_impure = v%value()
    end function
    ! C14: a PURE procedure cannot mutate a read-only view.
    pure subroutine bad_pure_step_readonly(v)
        class(IStep), intent(in) :: v
        call v%step()
    end subroutine
    ! C15: nor a read-only dummy of a constrained type.
    pure subroutine bad_pure_step_generic{IStep :: T}(x)
        type(T), intent(in) :: x
        call x%step()
    end subroutine

    integer function ok_observe(v)
        class(ICounter), intent(in) :: v
        ok_observe = v%value()
    end function
    ! A receiver without INTENT does not require a definable object.
    subroutine ok_touch_readonly(v)
        class(ICounter), intent(in) :: v
        call v%touch()
    end subroutine
    ! An intent(in) pointer fixes the association, not its target.
    subroutine ok_bump_pointer(p)
        class(ICounter), pointer, intent(in) :: p
        call p%bump(1)
        call p%reset(seed=2)
    end subroutine
    subroutine ok_bump_generic{ICounter :: T}(x)
        type(T), intent(inout) :: x
        call x%bump(by=1)
        call x%touch()
    end subroutine
    pure subroutine ok_pure_step(v)
        class(IStep), intent(inout) :: v
        call v%step()
        if (v%peek() < 0) error stop
    end subroutine
end module cc_traits_receiver_calls

! The same errors without traits.
module cc_traits_receiver_ordinary
    implicit none
    type :: Plain
        integer :: n = 0
    contains
        procedure :: mutate => plain_mutate
        procedure :: reset => plain_reset
    end type
contains
    subroutine plain_mutate(self)
        class(Plain), intent(inout) :: self
        self%n = self%n + 1
    end subroutine
    subroutine plain_reset(self)
        class(Plain), intent(out) :: self
        self%n = 0
    end subroutine
    ! C16: an intent(in) dummy as an intent(inout) passed object.
    subroutine ordinary_mutate_readonly(p)
        type(Plain), intent(in) :: p
        call p%mutate()
    end subroutine
    ! C17: an intent(in) dummy as an intent(out) passed object.
    subroutine ordinary_reset_readonly(p)
        class(Plain), intent(in) :: p
        call p%reset()
    end subroutine
end module cc_traits_receiver_ordinary

! PURE implementations follow the ordinary rules.
module cc_traits_receiver_pure_contracts
    implicit none
    abstract interface :: ITick
        pure subroutine tick(self)
            type(itself), intent(inout), pass :: self
        end subroutine
    end interface
end module cc_traits_receiver_pure_contracts

! C18: an impure procedure does not implement a PURE message.
module cc_traits_receiver_bad_impure_tick
    use cc_traits_receiver_pure_contracts, only: ITick
    implicit none
    type :: Clock
        integer :: n = 0
    end type
    implements ITick :: Clock
        procedure, pass :: tick => clock_tick
    end implements
contains
    subroutine clock_tick(self)
        class(Clock), intent(inout) :: self
        self%n = self%n + 1
    end subroutine
end module cc_traits_receiver_bad_impure_tick

! C19: a PURE function cannot have an intent(inout) dummy (C1583), so no
! procedure implements a PURE function message with an intent(inout)
! receiver; the rule also applies to the interface of the message.
module cc_traits_receiver_bad_pure_function_inout
    implicit none
    abstract interface :: ITake
        pure integer function take(self)
            type(itself), intent(inout), pass :: self
        end function
    end interface
    type, sealed :: Bag
        integer :: n = 0
    end type
    implements ITake :: Bag
        procedure, pass :: take => bag_take
    end implements
contains
    pure integer function bag_take(self)
        type(Bag), intent(inout) :: self
        bag_take = self%n
    end function
end module cc_traits_receiver_bad_pure_function_inout

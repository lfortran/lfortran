module traits_runtime_component_10_m
    implicit none
    integer :: finals = 0, final_sum = 0, holder_finals = 0, plain_finals = 0
    abstract interface :: IValue
        pure integer function value()
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
    type :: MutatingHolder
        class(IValue), allocatable :: item
    contains
        final :: finish_mutating
    end type
    type :: ReleasingHolder
        class(IValue), allocatable :: item
    contains
        final :: finish_releasing
    end type
    type :: Inner
        class(IValue), allocatable :: item
    contains
        final :: finish_inner
    end type
    type :: Outer
        type(Inner) :: inner
        integer :: tag = 0
    end type
    type :: Plain
        integer, allocatable :: values(:)
        integer :: n = 0
    contains
        final :: finish_plain
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
    subroutine finish_mutating(self)
        type(MutatingHolder), intent(inout) :: self
        holder_finals = holder_finals + 1
        if (.not. allocated(self%item)) return
        select type (p => self%item)
        type is (Payload)
            p%n = -91
        end select
    end subroutine
    subroutine finish_releasing(self)
        type(ReleasingHolder), intent(inout) :: self
        holder_finals = holder_finals + 1
        if (allocated(self%item)) deallocate(self%item)
    end subroutine
    subroutine finish_inner(self)
        type(Inner), intent(inout) :: self
        holder_finals = holder_finals + 1
        if (allocated(self%item)) deallocate(self%item)
    end subroutine
    subroutine finish_plain(self)
        type(Plain), intent(inout) :: self
        plain_finals = plain_finals + 1
        self%n = -1
        if (allocated(self%values)) deallocate(self%values)
    end subroutine
    subroutine check(ok, code)
        logical, intent(in) :: ok
        integer, intent(in) :: code
        if (.not. ok) error stop code
    end subroutine
end module

program traits_runtime_component_10
    use traits_runtime_component_10_m
    implicit none
    type(MutatingHolder) :: m, other
    type(ReleasingHolder) :: r
    type(Outer) :: o
    type(Plain) :: p
    type(Payload) :: seed

    ! F2023 7.5.6.3 finalizes the variable after expr is evaluated, so its
    ! final subroutine cannot change the value that redefines it.
    seed%n = 7
    m%item = seed
    m = m
    call check(holder_finals == 1, 1)
    call check(allocated(m%item), 2)
    call check(m%item%value() == 7, 3)
    call check(finals == 1 .and. final_sum == -91, 4)

    seed%n = 4
    other%item = seed
    other = m
    call check(holder_finals == 2, 5)
    call check(other%item%value() == 7 .and. m%item%value() == 7, 6)
    call check(finals == 2 .and. final_sum == -182, 7)

    seed%n = 5
    r%item = seed
    r = r
    call check(holder_finals == 3, 8)
    call check(allocated(r%item), 9)
    call check(r%item%value() == 5, 10)
    call check(finals == 3 .and. final_sum == -177, 11)

    seed%n = 12
    o%inner%item = seed
    o%tag = 3
    o = o
    call check(holder_finals == 4, 12)
    call check(allocated(o%inner%item), 13)
    call check(o%inner%item%value() == 12 .and. o%tag == 3, 14)
    call check(finals == 4 .and. final_sum == -165, 15)

    allocate(p%values(3))
    p%values = [1, 2, 3]
    p%n = 5
    p = p
    call check(plain_finals == 1, 16)
    call check(allocated(p%values), 17)
    call check(all(p%values == [1, 2, 3]) .and. p%n == 5, 18)

    deallocate(m%item, other%item, r%item, o%inner%item)
    call check(finals == 8 .and. final_sum == -134, 19)
    call check(holder_finals == 4, 20)
end program

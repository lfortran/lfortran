module traits_runtime_component_17_types
    implicit none
    integer :: finals = 0, final_sum = 0
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
    type :: Holder
        class(IValue), allocatable :: item
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
    subroutine reset()
        finals = 0
        final_sum = 0
    end subroutine
end module

! The client imports only the types, so each procedure imports the contract,
! slot procedures and inspection types it uses into its own scopes.
module traits_runtime_component_17_client
    use traits_runtime_component_17_types, only: Holder, Payload
    implicit none
contains
    subroutine fill(x, base)
        type(Holder), intent(inout) :: x(:)
        integer, intent(in) :: base
        type(Payload) :: seed
        integer :: i
        do i = 1, size(x)
            seed%n = base + i
            if (mod(i, 2) == 0) then
                x(i)%item = seed
            else
                if (allocated(x(i)%item)) deallocate(x(i)%item)
                allocate(x(i)%item, source=seed)
            end if
        end do
    end subroutine
    integer function inspect(x) result(r)
        type(Holder), intent(in) :: x(:)
        integer :: i
        r = 0
        do i = 1, size(x)
            select type (item => x(i)%item)
            type is (Payload)
                r = r * 10 + item%n
            class default
                r = -1
            end select
        end do
    end function
    integer function staged(x) result(r)
        type(Holder), intent(in) :: x(:)
        r = 0
        block
            type(Holder) :: local(2)
            local(1) = x(1)
            local(2) = x(size(x))
            associate (first => local(1))
                r = first%item%value() * 100 + local(2)%item%value()
            end associate
        end block
    end function
    integer function packed(x, mask) result(r)
        type(Holder), intent(in) :: x(:)
        logical, intent(in) :: mask(:)
        r = size(pack(x, mask))
    end function
    subroutine move_all(a, b)
        type(Holder), allocatable, intent(inout) :: a(:), b(:)
        call move_alloc(a, b)
    end subroutine
    subroutine clear(x)
        type(Holder), intent(inout) :: x(:)
        integer :: i
        do i = 1, size(x)
            deallocate(x(i)%item)
        end do
    end subroutine
end module

program traits_runtime_component_17
    use traits_runtime_component_17_types
    use traits_runtime_component_17_client
    implicit none
    type(Holder) :: p(3)
    type(Holder), allocatable :: a(:), b(:)

    ! Each call to fill also finalizes its local seed on return.
    call fill(p, 0)
    if (finals /= 1 .or. final_sum /= 3) error stop 1
    call reset()
    call fill(p, 3)
    if (finals /= 4 .or. final_sum /= 12) error stop 2
    call reset()
    if (inspect(p) /= 456 .or. finals /= 0) error stop 3

    ! Copies in a BLOCK and in the temporary result of PACK are finalized
    ! with them; the elements of p are not.
    if (staged(p) /= 406) error stop 4
    if (finals /= 2 .or. final_sum /= 10) error stop 5
    call reset()
    if (packed(p, [.true., .false., .true.]) /= 2) error stop 6
    if (finals /= 2 .or. final_sum /= 10) error stop 7
    if (inspect(p) /= 456) error stop 8

    ! MOVE_ALLOC of arrays finalizes only the old payloads of TO.
    allocate(a(2), b(3))
    call fill(a, 10)
    call fill(b, 20)
    call reset()
    call move_all(a, b)
    if (finals /= 3 .or. final_sum /= 66) error stop 9
    if (allocated(a) .or. size(b) /= 2) error stop 10
    if (b(1)%item%value() /= 11 .or. b(2)%item%value() /= 12) error stop 11

    call reset()
    call clear(p)
    if (finals /= 3 .or. final_sum /= 15) error stop 12
    call reset()
    deallocate(b)
    if (finals /= 2 .or. final_sum /= 23) error stop 13
end program

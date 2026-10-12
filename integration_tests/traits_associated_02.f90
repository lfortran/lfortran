! Mutating trait methods (paper sect:tats and sect:associated_types_usage):
! IAppendable's append(item) declares no receiver; Vector{U} implements it
! exactly as in the paper with an INTENT(INOUT) passed-object dummy, and the
! non-generic Tally implements it with Element = integer. Appends are made
! on variables, through an INTENT(INOUT) dummy and on a nested generic
! instance. Direct calls on concrete objects may mutate their receiver
! (PAPER-D07). Each unit uses only the instances it owns: the INTENT(INOUT)
! dummy is appended to in the module that creates its Vector{integer}.
module traits_associated_02_traits_m
    implicit none
    private
    public :: IAppendable

    abstract interface :: IAppendable
        type :: Element
        subroutine append(item)
            type(Element), intent(in) :: item
        end subroutine append
    end interface IAppendable
end module traits_associated_02_traits_m

module traits_associated_02_vector_m
    use traits_associated_02_traits_m, only: IAppendable
    implicit none
    private

    type, public, sealed, implements(IAppendable) :: Vector{U}
        private
        type(U), allocatable :: elements(:)
    contains
        initial :: init
        procedure, pass :: append
        procedure, pass :: length
        procedure, pass :: get
    end type Vector

contains

    function init(item) result(res)
        type(U), intent(in) :: item
        type(Vector{U})     :: res
        res%elements = [item]
    end function init

    subroutine append(self, item)
        type(Vector{U}), intent(inout) :: self
        type(U),         intent(in)    :: item
        self%elements = [self%elements, item]
    end subroutine append

    function length(self) result(n)
        type(Vector{U}), intent(in) :: self
        integer                     :: n
        n = size(self%elements)
    end function length

    function get(self, i) result(item)
        type(Vector{U}), intent(in) :: self
        integer,         intent(in) :: i
        type(U)                     :: item
        item = self%elements(i)
    end function get
end module traits_associated_02_vector_m

module traits_associated_02_tally_m
    use traits_associated_02_traits_m, only: IAppendable
    implicit none
    private

    type, public, sealed, implements(IAppendable) :: Tally
        private
        integer :: total = 0
        integer :: items = 0
    contains
        procedure, pass :: append => add_item
        procedure, pass :: total_of
        procedure, pass :: count_of
    end type Tally

contains

    subroutine add_item(self, item)
        type(Tally), intent(inout) :: self
        integer,     intent(in)    :: item
        self%total = self%total + item
        self%items = self%items + 1
    end subroutine add_item

    function total_of(self) result(n)
        type(Tally), intent(in) :: self
        integer                 :: n
        n = self%total
    end function total_of

    function count_of(self) result(n)
        type(Tally), intent(in) :: self
        integer                 :: n
        n = self%items
    end function count_of
end module traits_associated_02_tally_m

module traits_associated_02_use_m
    use traits_associated_02_vector_m, only: Vector
    implicit none
    private
    public :: grown

contains

    subroutine grow(v, n)
        type(Vector{integer}), intent(inout) :: v
        integer,               intent(in)    :: n
        integer                              :: i
        do i = 1, n
            call v%append(v%length() + 1)
        end do
    end subroutine grow

    ! Fifty appends through the passed-object dummy and through a dummy.
    subroutine grown(length, last, total)
        integer, intent(out)  :: length, last, total
        type(Vector{integer}) :: ints
        integer               :: i
        ints = Vector{integer}(1)
        do i = 2, 25
            call ints%append(i)
        end do
        call grow(ints, 25)
        total = 0
        do i = 1, ints%length()
            total = total + ints%get(i)
        end do
        length = ints%length()
        last = ints%get(length)
    end subroutine grown
end module traits_associated_02_use_m

program traits_associated_02
    use traits_associated_02_vector_m, only: Vector
    use traits_associated_02_tally_m, only: Tally
    use traits_associated_02_use_m, only: grown
    implicit none
    integer :: checks, length, last, total
    type(Vector{integer}) :: inner
    type(Tally) :: t

    checks = 0

    doubles := Vector(0.d0)
    call doubles%append(1.5d0)
    call doubles%append(2.2d0)
    call check(doubles%length() == 3, 1)
    call check(doubles%get(1) == 0.d0 .and. doubles%get(2) == 1.5d0 &
        .and. doubles%get(3) == 2.2d0, 2)
    bools := Vector{logical}(.true.)
    call bools%append(.false.)
    call bools%append(.true.)
    call check(bools%length() == 3, 3)
    call check(bools%get(1) .and. .not. bools%get(2) .and. bools%get(3), 4)
    strings := Vector("John ")
    call strings%append("Mary ")
    call strings%append("Anne ")
    call check(strings%length() == 3 .and. len(strings%get(3)) == 5, 5)
    call check(strings%get(1) // strings%get(2) // strings%get(3) == "John Mary Anne ", 6)

    call grown(length, last, total)
    call check(length == 50 .and. last == 50, 7)
    call check(total == 1275, 8)

    ! U is itself an instance of the generic type.
    nested := Vector(Vector(1))
    call nested%append(Vector(2))
    inner = nested%get(2)
    call inner%append(3)
    call check(nested%length() == 2 .and. inner%length() == 2, 9)
    inner = nested%get(2)
    call check(inner%length() == 1 .and. inner%get(1) == 2, 10)

    ! Element = integer for a non-generic implementing type.
    call t%append(5)
    call t%append(7)
    call check(t%total_of() == 12 .and. t%count_of() == 2, 11)

    print '(a,i0)', 'traits_associated_02 checks: ', checks
    if (checks /= 11) error stop 99

contains

    subroutine check(condition, id)
        logical, intent(in) :: condition
        integer, intent(in) :: id
        if (.not. condition) then
            print '(a,i0)', 'traits_associated_02 failed check ', id
            error stop 1
        end if
        checks = checks + 1
    end subroutine check
end program traits_associated_02

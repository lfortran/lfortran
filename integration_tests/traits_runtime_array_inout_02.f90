! LFortran traits extension test; GFortran does not accept this syntax.
! Assumed-shape array arguments with every ordinary INTENT in runtime messages
! that are not generic: intent(inout), intent(out) with a declared lower bound
! and no INTENT, of real(8), integer, character and logical arrays, with a
! read-only or an intent(inout) receiver, and in a function message. Through
! an owner, a pointer and forwarding procedures, whole arrays and strided,
! reversed, row and pointer sections, allocatable arrays and assumed-shape
! dummies reach the implementation as descriptors of the caller's storage:
! the contiguity probe sees strided actuals as noncontiguous, every element
! of a section is defined in place, and the elements outside it keep their
! values.
module traits_runtime_array_inout_02_m
    implicit none
    private
    public :: IArrays, Scaler, scale_through, drain_through

    abstract interface :: IArrays
        subroutine scale(self, x, contiguous)
            type(itself), pass, intent(in) :: self
            real(8), intent(inout) :: x(:)
            logical, intent(out) :: contiguous
        end subroutine scale
        subroutine number(self, x, first)
            type(itself), pass, intent(in) :: self
            real(8), intent(out) :: x(0:)
            integer, intent(out) :: first
        end subroutine number
        subroutine shift(self, m)
            type(itself), pass, intent(in) :: self
            integer :: m(:, :)
        end subroutine shift
        subroutine mark(self, tags, flags)
            type(itself), pass, intent(in) :: self
            character(len=2), intent(inout) :: tags(:)
            logical, intent(inout) :: flags(:)
        end subroutine mark
        subroutine drain(self, x)
            type(itself), pass, intent(inout) :: self
            real(8), intent(inout) :: x(:)
        end subroutine drain
        integer function clear(self, x) result(n)
            type(itself), pass, intent(in) :: self
            integer, intent(inout) :: x(:)
        end function clear
    end interface IArrays

    type :: Scaler
        real(8) :: factor = 2
        real(8) :: drained = 0
    end type Scaler
    implements IArrays :: Scaler
        procedure, pass :: scale => scaler_scale
        procedure, pass :: number => scaler_number
        procedure, pass :: shift => scaler_shift
        procedure, pass :: mark => scaler_mark
        procedure, pass :: drain => scaler_drain
        procedure, pass :: clear => scaler_clear
    end implements

contains

    subroutine scaler_scale(self, x, contiguous)
        class(Scaler), intent(in) :: self
        real(8), intent(inout) :: x(:)
        logical, intent(out) :: contiguous
        contiguous = is_contiguous(x)
        x = self%factor*x
    end subroutine scaler_scale

    ! x(i) <- 10 i from the declared lower bound 0.
    subroutine scaler_number(self, x, first)
        class(Scaler), intent(in) :: self
        real(8), intent(out) :: x(0:)
        integer, intent(out) :: first
        integer :: i
        first = lbound(x, 1)
        do i = 0, ubound(x, 1)
            x(i) = 10*i
        end do
    end subroutine scaler_number

    subroutine scaler_shift(self, m)
        class(Scaler), intent(in) :: self
        integer :: m(:, :)
        m = m + nint(self%factor)
    end subroutine scaler_shift

    subroutine scaler_mark(self, tags, flags)
        class(Scaler), intent(in) :: self
        character(len=2), intent(inout) :: tags(:)
        logical, intent(inout) :: flags(:)
        integer :: i
        do i = 1, size(tags)
            tags(i) = tags(i)(2:2) // tags(i)(1:1)
        end do
        flags = .not. flags
    end subroutine scaler_mark

    ! The receiver keeps the total of what it removes.
    subroutine scaler_drain(self, x)
        class(Scaler), intent(inout) :: self
        real(8), intent(inout) :: x(:)
        self%drained = self%drained + sum(x)
        x = 0
    end subroutine scaler_drain

    integer function scaler_clear(self, x) result(n)
        class(Scaler), intent(in) :: self
        integer, intent(inout) :: x(:)
        n = count(x /= 0)
        x = 0
    end function scaler_clear

    ! A view and an assumed-shape dummy forwarded to the message.
    subroutine scale_through(view, x, contiguous)
        class(IArrays), intent(in) :: view
        real(8), intent(inout) :: x(:)
        logical, intent(out) :: contiguous
        call view%scale(x, contiguous)
    end subroutine scale_through

    subroutine drain_through(view, x)
        class(IArrays), intent(inout) :: view
        real(8), intent(inout) :: x(:)
        call view%drain(x)
    end subroutine drain_through
end module traits_runtime_array_inout_02_m

program traits_runtime_array_inout_02
    use traits_runtime_array_inout_02_m, only: IArrays, Scaler, scale_through, &
        drain_through
    implicit none
    class(IArrays), allocatable :: owner
    class(IArrays), pointer :: view
    type(Scaler), target :: concrete
    real(8) :: a(9), g(3, 4)
    real(8), allocatable :: h(:)
    real(8), pointer :: p(:)
    real(8), target :: storage(8)
    integer :: m(3, 4), k(5), first, n, i
    character(len=2) :: tags(3)
    logical :: flags(4), contiguous

    allocate(owner, source=Scaler(factor=3))
    concrete%factor = -1
    view => concrete

    ! intent(inout): whole, strided, reversed (by keyword) and row sections.
    a = [(real(i, 8), i = 1, 9)]
    call owner%scale(a, contiguous)
    if (.not. contiguous) error stop 1
    if (any(a /= [(3*real(i, 8), i = 1, 9)])) error stop 2
    a = [(real(i, 8), i = 1, 9)]
    call owner%scale(a(1:9:2), contiguous)
    if (contiguous) error stop 3
    if (any(a /= [3d0, 2d0, 9d0, 4d0, 15d0, 6d0, 21d0, 8d0, 27d0])) error stop 4
    a = [(real(i, 8), i = 1, 9)]
    call owner%scale(contiguous=contiguous, x=a(8:2:-3))
    if (contiguous) error stop 5
    if (any(a /= [1d0, 6d0, 3d0, 4d0, 15d0, 6d0, 7d0, 24d0, 9d0])) error stop 6
    g = reshape([(real(i, 8), i = 1, 12)], [3, 4])
    call view%scale(g(2, :), contiguous)
    if (contiguous) error stop 7
    if (any(g(2, :) /= [-2d0, -5d0, -8d0, -11d0]) .or. any(g(1, :) /= [1d0, 4d0, 7d0, 10d0]) &
        .or. any(g(3, :) /= [3d0, 6d0, 9d0, 12d0])) error stop 8

    ! An allocatable array and a pointer to a strided section.
    allocate(h(4))
    h = [1d0, 2d0, 3d0, 4d0]
    call owner%scale(h, contiguous)
    if (.not. contiguous .or. any(h /= [3d0, 6d0, 9d0, 12d0])) error stop 9
    storage = [(real(i, 8), i = 1, 8)]
    p => storage(2:8:3)
    call view%scale(p, contiguous)
    if (contiguous) error stop 10
    if (any(storage /= [1d0, -2d0, 3d0, 4d0, -5d0, 6d0, 7d0, -8d0])) error stop 11

    ! Forwarded assumed-shape dummies stay descriptors of the caller's array.
    a = [(real(i, 8), i = 1, 9)]
    call scale_through(owner, a(9:1:-2), contiguous)
    if (contiguous) error stop 12
    if (any(a /= [3d0, 2d0, 9d0, 4d0, 15d0, 6d0, 21d0, 8d0, 27d0])) error stop 13

    ! intent(out) with a declared lower bound, contiguous and reversed strided.
    a = -1
    call owner%number(a(3:7), first)
    if (first /= 0) error stop 14
    if (any(a /= [-1d0, -1d0, 0d0, 10d0, 20d0, 30d0, 40d0, -1d0, -1d0])) error stop 15
    a = -1
    call view%number(a(9:1:-4), first)
    if (first /= 0) error stop 16
    if (any(a /= [20d0, -1d0, -1d0, -1d0, 10d0, -1d0, -1d0, -1d0, 0d0])) error stop 17

    ! No INTENT: a rank-2 integer array and a noncontiguous section of it.
    m = reshape([(i, i = 1, 12)], [3, 4])
    call owner%shift(m)
    if (any(m /= reshape([(i + 3, i = 1, 12)], [3, 4]))) error stop 18
    call owner%shift(m(1:3:2, 2:4))
    if (any(m(:, 1) /= [4, 5, 6]) .or. any(m(1, 2:4) /= [10, 13, 16]) .or. &
        any(m(2, 2:4) /= [8, 11, 14]) .or. any(m(3, 2:4) /= [12, 15, 18])) error stop 19

    ! intent(inout) character and logical sections.
    tags = ['ab', 'cd', 'ef']
    flags = [.true., .false., .true., .false.]
    call owner%mark(tags(3:1:-2), flags(2:4:2))
    if (any(tags /= ['ba', 'cd', 'fe'])) error stop 20
    if (any(flags .neqv. [.true., .true., .true., .true.])) error stop 21
    call view%mark(tags, flags)
    if (any(tags /= ['ab', 'dc', 'ef'])) error stop 22
    if (any(flags)) error stop 23

    ! An intent(inout) receiver with an intent(inout) array: the owner's and
    ! the pointer target's own state.
    a = [(real(i, 8), i = 1, 9)]
    call owner%drain(a(2:6))
    if (any(a /= [1d0, 0d0, 0d0, 0d0, 0d0, 0d0, 7d0, 8d0, 9d0])) error stop 24
    call drain_through(view, a(9:7:-1))
    if (any(a(7:9) /= 0d0) .or. a(1) /= 1d0) error stop 25
    if (concrete%drained /= 24d0 .or. concrete%factor /= -1d0) error stop 26
    select type (s => owner)
    type is (Scaler)
        if (s%drained /= 20d0 .or. s%factor /= 3d0) error stop 27
    class default
        error stop 28
    end select

    ! A function message with an intent(inout) array.
    k = [1, 7, 0, 7, 5]
    n = owner%clear(k(1:5:2))
    if (n /= 2 .or. any(k /= [0, 7, 0, 7, 0])) error stop 29
    n = view%clear(k)
    if (n /= 2 .or. any(k /= 0)) error stop 30
    print '(a)', 'traits_runtime_array_inout_02 ok'
end program traits_runtime_array_inout_02

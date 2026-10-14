! LFortran traits extension test; GFortran does not accept this syntax.
! PURE messages with explicit receivers (TD-36): a PURE function with an
! intent(in) receiver and PURE subroutines with intent(inout) and intent(out)
! receivers, implemented by PURE procedures. As for any PURE procedure, the
! intent(out) passed object is not polymorphic: it is the TYPE of a sealed
! type, whose allocatable component is deallocated and whose default
! initialization is applied on entry. PURE procedures call the messages
! through a constrained type parameter and through trait objects, and an
! elemental function reads the receiver of each element.
! traits_receiver_intent_05_oracle.f90 is the standard-Fortran counterpart.
module traits_receiver_intent_05_m
    implicit none
    abstract interface :: ITally
        pure integer function total(self)
            type(itself), intent(in), pass :: self
        end function
        pure subroutine record(x, self)
            integer, intent(in) :: x
            type(itself), intent(inout), pass :: self
        end subroutine
        pure subroutine clear(self)
            type(itself), intent(out), pass :: self
        end subroutine
    end interface
    type, sealed :: Tally
        integer :: sum = 0
        integer :: count = 0
        integer, allocatable :: last(:)
    end type
    implements ITally :: Tally
        procedure, pass :: total => tally_total
        procedure, pass :: record => tally_record
        procedure, pass :: clear => tally_clear
    end implements
contains
    pure integer function tally_total(self)
        type(Tally), intent(in) :: self
        tally_total = self%sum
    end function
    pure subroutine tally_record(self, x)
        type(Tally), intent(inout) :: self
        integer, intent(in) :: x
        self%sum = self%sum + x
        self%count = self%count + 1
        if (allocated(self%last)) deallocate(self%last)
        allocate(self%last(1))
        self%last(1) = x
    end subroutine
    pure subroutine tally_clear(self)
        type(Tally), intent(out) :: self
        if (self%sum /= 0 .or. self%count /= 0 .or. allocated(self%last)) error stop 90
        self%count = -1
    end subroutine
    pure subroutine feed_generic{ITally :: T}(acc, xs)
        type(T), intent(inout) :: acc
        integer, intent(in) :: xs(:)
        integer :: i
        call acc%clear()
        do i = 1, size(xs)
            call acc%record(xs(i))
        end do
    end subroutine
    pure integer function total_generic{ITally :: T}(acc)
        type(T), intent(in) :: acc
        total_generic = acc%total()
    end function
    pure subroutine feed_view(t, xs)
        class(ITally), intent(inout) :: t
        integer, intent(in) :: xs(:)
        integer :: i
        call t%clear()
        do i = 1, size(xs)
            call t%record(x=xs(i))
        end do
    end subroutine
    pure integer function total_view(t)
        class(ITally), intent(in) :: t
        total_view = t%total()
    end function
    elemental integer function doubled(t)
        type(Tally), intent(in) :: t
        doubled = 2 * t%total()
    end function
end module

program traits_receiver_intent_05
    use traits_receiver_intent_05_m
    implicit none
    type(Tally) :: a, b, many(3)
    integer :: d(3), i
    call a%record(5)
    call a%record(7)
    if (a%total() /= 12 .or. a%count /= 2 .or. a%last(1) /= 7) error stop 1
    call feed_generic(a, [1, 2, 3])
    if (total_generic(a) /= 6 .or. a%count /= 2 .or. a%last(1) /= 3) error stop 2
    call b%record(100)
    call feed_view(b, [4, 5])
    if (total_view(b) /= 9 .or. b%count /= 1 .or. b%last(1) /= 5) error stop 3
    call b%clear()
    if (b%total() /= 0 .or. b%count /= -1 .or. allocated(b%last)) error stop 4
    do i = 1, 3
        call feed_view(many(i), [i, 10 * i])
    end do
    d = doubled(many)
    if (any(d /= [22, 44, 66])) error stop 5
    print '(a)', 'traits_receiver_intent_05 ok'
end program

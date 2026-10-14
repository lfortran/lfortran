! Standard-Fortran counterpart of traits_receiver_intent_05.f90: the PURE
! procedures of the trait test called directly on a nonpolymorphic type, as
! a PURE procedure's intent(out) dummy cannot be polymorphic. It checks the
! same values, including the deallocation and default initialization of an
! intent(out) argument on entry.
module traits_receiver_intent_05_oracle_m
    implicit none
    type :: Tally
        integer :: sum = 0
        integer :: count = 0
        integer, allocatable :: last(:)
    end type
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
    pure subroutine feed(t, xs)
        type(Tally), intent(inout) :: t
        integer, intent(in) :: xs(:)
        integer :: i
        call tally_clear(t)
        do i = 1, size(xs)
            call tally_record(t, x=xs(i))
        end do
    end subroutine
    elemental integer function doubled(t)
        type(Tally), intent(in) :: t
        doubled = 2 * tally_total(t)
    end function
end module

program traits_receiver_intent_05_oracle
    use traits_receiver_intent_05_oracle_m
    implicit none
    type(Tally) :: a, b, many(3)
    integer :: d(3), i
    call tally_record(a, 5)
    call tally_record(a, 7)
    if (tally_total(a) /= 12 .or. a%count /= 2 .or. a%last(1) /= 7) error stop 1
    call feed(a, [1, 2, 3])
    if (tally_total(a) /= 6 .or. a%count /= 2 .or. a%last(1) /= 3) error stop 2
    call tally_record(b, 100)
    call feed(b, [4, 5])
    if (tally_total(b) /= 9 .or. b%count /= 1 .or. b%last(1) /= 5) error stop 3
    call tally_clear(b)
    if (tally_total(b) /= 0 .or. b%count /= -1 .or. allocated(b%last)) error stop 4
    do i = 1, 3
        call feed(many(i), [i, 10 * i])
    end do
    d = doubled(many)
    if (any(d /= [22, 44, 66])) error stop 5
    print '(a)', 'traits_receiver_intent_05 ok'
end program

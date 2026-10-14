! LFortran traits extension test; GFortran does not accept this syntax.
! intent(out) receivers of a dynamic type that extends the implementing type
! (TD-36): Gauge2 extends Gauge, adopts IReset too and inherits Gauge's
! bindings, whose passed objects are class(Gauge) with intent(out). Each call
! of reset, direct, through an intent(inout) trait-object dummy, through a
! static generic call or through an allocatable owner, finalizes the object
! once for its dynamic type (Gauge2's final subroutine, then its parent's),
! deallocates the allocatable components of Gauge2 and of its parent and
! applies their default initialization before the body runs. The object keeps
! its address and the owner its allocation and dynamic type. The ordinary
! polymorphic intent(out) dummies of traits_receiver_intent_04_oracle.f90
! check the same values.
module traits_receiver_intent_04_m
    use iso_c_binding, only: c_ptr, c_loc, c_associated, c_null_ptr
    implicit none
    integer :: finals = 0, finals2 = 0, entries = 0
    type(c_ptr) :: expected = c_null_ptr
    abstract interface :: IReset
        integer function level(self)
            type(itself), intent(in), pass :: self
        end function
        subroutine reset(self, start)
            type(itself), intent(out), pass :: self
            integer, intent(in) :: start
        end subroutine
    end interface
    type, implements(IReset) :: Gauge
        integer :: n = 7
        integer, allocatable :: data(:)
    contains
        procedure, pass :: level => gauge_level
        procedure, pass :: reset => gauge_reset
        final :: gauge_final
    end type
    type, extends(Gauge), implements(IReset) :: Gauge2
        integer :: extra = 11
        integer, allocatable :: more(:)
    contains
        final :: gauge2_final
    end type
contains
    integer function gauge_level(self)
        class(Gauge), intent(in) :: self
        gauge_level = self%n
    end function
    subroutine gauge_reset(self, start)
        class(Gauge), target, intent(out) :: self
        integer, intent(in) :: start
        type(c_ptr) :: actual
        actual = c_loc(self%n)
        if (c_associated(expected)) then
            if (.not. c_associated(actual, expected)) error stop 90
        end if
        if (self%n /= 7 .or. allocated(self%data)) error stop 91
        select type (self)
        type is (Gauge2)
            if (self%extra /= 11 .or. allocated(self%more)) error stop 92
            self%extra = 2 * start
            allocate(self%more(start))
        class default
            error stop 93
        end select
        entries = entries + 1
        self%n = start
        allocate(self%data(start))
    end subroutine
    subroutine gauge_final(self)
        type(Gauge), intent(inout) :: self
        finals = finals + 1
    end subroutine
    subroutine gauge2_final(self)
        type(Gauge2), intent(inout) :: self
        finals2 = finals2 + 1
    end subroutine
    subroutine reset_view(x, start)
        class(IReset), intent(inout) :: x
        integer, intent(in) :: start
        call x%reset(start)
    end subroutine
    subroutine reset_generic{IReset :: T}(x, start)
        type(T), intent(inout) :: x
        integer, intent(in) :: start
        call x%reset(start)
    end subroutine
    subroutine expect(g, n)
        class(IReset), intent(in) :: g
        integer, intent(in) :: n
        if (finals /= 1 .or. finals2 /= 1 .or. entries /= 1) then
            print '(a,3i4)', 'counts:', finals, finals2, entries
            error stop 94
        end if
        finals = 0
        finals2 = 0
        entries = 0
        select type (g)
        type is (Gauge2)
            if (g%n /= n .or. g%extra /= 2 * n) error stop 95
            if (size(g%data) /= n .or. size(g%more) /= n) error stop 96
        class default
            error stop 97
        end select
    end subroutine
end module

program traits_receiver_intent_04
    use iso_c_binding, only: c_loc
    use traits_receiver_intent_04_m
    implicit none
    type(Gauge2), target :: g2
    class(IReset), allocatable, target :: owner

    g2%n = 1
    g2%extra = 1
    allocate(g2%data(2), g2%more(2))
    expected = c_loc(g2%n)
    call g2%reset(3)
    call expect(g2, 3)
    call reset_view(g2, 4)
    call expect(g2, 4)
    call reset_generic(g2, 5)
    call expect(g2, 5)
    allocate(Gauge2 :: owner)
    select type (concrete => owner)
    type is (Gauge2)
        expected = c_loc(concrete%n)
    class default
        error stop 1
    end select
    call owner%reset(6)
    call expect(owner, 6)
    call reset_view(owner, 2)
    call expect(owner, 2)
    if (.not. allocated(owner) .or. owner%level() /= 2) error stop 2
    print '(a)', 'traits_receiver_intent_04 ok'
end program

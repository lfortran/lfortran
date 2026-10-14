! Standard-Fortran counterpart of traits_receiver_intent_09.f90: Gauge2
! extends Gauge, which implements the deferred binding reset of IReset with a
! class(Gauge) passed object with intent(out). Each call finalizes the object
! once for its dynamic type (Gauge2's final subroutine, then its parent's),
! deallocates the allocatable components of Gauge2 and of its parent and
! applies their default initialization before the body runs, which checks the
! order and counts of the finalizations on entry and the old values and
! allocation status the final subroutines saw, through direct calls,
! class(IReset) dummies, a pointer and an allocatable owner.
module traits_receiver_intent_09_oracle_m
    use iso_c_binding, only: c_ptr, c_loc, c_associated, c_null_ptr
    implicit none
    integer :: finals = 0, finals2 = 0, entries = 0
    ! Finalization order: Gauge2's final subroutine appends 2, Gauge's 1.
    integer :: order = 0
    ! What the final subroutines saw: old values and allocation status.
    integer :: seen_n = -1, seen_extra = -1, seen_allocated = 0
    type(c_ptr) :: expected = c_null_ptr
    type, abstract :: IReset
    contains
        procedure(level_iface), deferred :: level
        procedure(reset_iface), deferred :: reset
    end type
    abstract interface
        integer function level_iface(self)
            import :: IReset
            class(IReset), intent(in) :: self
        end function
        subroutine reset_iface(self, start)
            import :: IReset
            class(IReset), target, intent(out) :: self
            integer, intent(in) :: start
        end subroutine
    end interface
    type, extends(IReset) :: Gauge
        integer :: n = 7
        integer, allocatable :: data(:)
    contains
        procedure, pass :: level => gauge_level
        procedure, pass :: reset => gauge_reset
        final :: gauge_final
    end type
    type, extends(Gauge) :: Gauge2
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
        ! The object was finalized once, child before parent, before the body.
        if (order /= 21 .or. finals /= 1 .or. finals2 /= 1) then
            print '(a,3i4)', 'entry:', order, finals, finals2
            error stop 89
        end if
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
        order = 10 * order + 1
        seen_n = self%n
        if (allocated(self%data)) seen_allocated = seen_allocated + 10
    end subroutine
    subroutine gauge2_final(self)
        type(Gauge2), intent(inout) :: self
        finals2 = finals2 + 1
        order = 10 * order + 2
        seen_extra = self%extra
        if (allocated(self%more)) seen_allocated = seen_allocated + 1
    end subroutine
    subroutine reset_view(x, start)
        class(IReset), intent(inout) :: x
        integer, intent(in) :: start
        call x%reset(start)
    end subroutine
    subroutine reset_generic(x, start)
        class(IReset), intent(inout) :: x
        integer, intent(in) :: start
        call x%reset(start)
    end subroutine
    subroutine expect(g, n, old_n, old_extra, old_allocated)
        class(IReset), intent(in) :: g
        integer, intent(in) :: n
        ! The values and allocation status before the call, which the final
        ! subroutines saw (Gauge's data counts 10, Gauge2's more 1).
        integer, intent(in) :: old_n, old_extra, old_allocated
        if (order /= 21 .or. finals /= 1 .or. finals2 /= 1 .or. entries /= 1) then
            print '(a,4i4)', 'counts:', order, finals, finals2, entries
            error stop 94
        end if
        if (seen_n /= old_n .or. seen_extra /= old_extra .or. &
                seen_allocated /= old_allocated) then
            print '(a,3i6)', 'seen:', seen_n, seen_extra, seen_allocated
            error stop 98
        end if
        order = 0
        finals = 0
        finals2 = 0
        entries = 0
        seen_n = -1
        seen_extra = -1
        seen_allocated = 0
        select type (g)
        type is (Gauge2)
            if (g%n /= n .or. g%extra /= 2 * n) error stop 95
            if (size(g%data) /= n .or. size(g%more) /= n) error stop 96
        class default
            error stop 97
        end select
    end subroutine
end module

program traits_receiver_intent_09_oracle
    use iso_c_binding, only: c_loc
    use traits_receiver_intent_09_oracle_m
    implicit none
    type(Gauge2), target :: g2
    class(IReset), allocatable, target :: owner
    class(IReset), pointer :: p

    g2%n = 1
    g2%extra = 1
    allocate(g2%data(2), g2%more(2))
    expected = c_loc(g2%n)
    call g2%reset(3)
    call expect(g2, 3, 1, 1, 11)
    call reset_view(g2, 4)
    call expect(g2, 4, 3, 6, 11)
    call reset_generic(g2, 5)
    call expect(g2, 5, 4, 8, 11)
    p => g2
    call p%reset(8)
    call expect(p, 8, 5, 10, 11)
    call reset_view(p, 9)
    call expect(p, 9, 8, 16, 11)
    allocate(Gauge2 :: owner)
    select type (concrete => owner)
    type is (Gauge2)
        expected = c_loc(concrete%n)
    class default
        error stop 1
    end select
    call owner%reset(6)
    call expect(owner, 6, 7, 11, 0)
    call reset_view(owner, 2)
    call expect(owner, 2, 6, 12, 11)
    if (.not. allocated(owner) .or. owner%level() /= 2) error stop 2
    print '(a)', 'traits_receiver_intent_09 ok'
end program

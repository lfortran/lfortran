! LFortran traits extension test; GFortran does not accept this syntax.
! intent(out) receivers (TD-36). On each call of reset or restart, whose
! receivers IReset declares intent(out), the object is finalized exactly once
! (the final subroutine still sees its old allocatable component), its
! allocatable components are deallocated and its default initialization is
! applied before the body of the implementation runs. The object keeps its
! address, its owner's allocation and its dynamic type. The paths are direct
! calls, an intent(inout) trait-object dummy borrowed from a variable, a
! static generic call, an allocatable owner and its component, a pointer, a
! function with an intent(out) receiver and a sealed type's TYPE passed
! object. The scratch component has no default initialization and is never
! read after entry. traits_receiver_intent_09 checks dynamic types that extend
! the implementing type. traits_receiver_intent_03_oracle.f90 is the
! standard-Fortran counterpart.
module traits_receiver_intent_03_m
    use iso_c_binding, only: c_ptr, c_loc, c_associated, c_null_ptr
    implicit none
    integer :: finals = 0, finals_with_data = 0, entries = 0
    type(c_ptr) :: expected = c_null_ptr
    abstract interface :: IReset
        integer function level(self)
            type(itself), intent(in), pass :: self
        end function
        subroutine reset(self, start)
            type(itself), intent(out), pass :: self
            integer, intent(in) :: start
        end subroutine
        integer function restart(start, self)
            integer, intent(in) :: start
            type(itself), intent(out), pass :: self
        end function
    end interface
    type, implements(IReset) :: Gauge
        integer :: n = 7
        integer :: scratch
        integer, allocatable :: data(:)
    contains
        procedure, pass :: level => gauge_level
        procedure, pass :: reset => gauge_reset
        procedure, pass(self) :: restart => gauge_restart
        final :: gauge_final
    end type
    type, sealed :: Seal
        integer :: n = 3
        integer, allocatable :: data(:)
    end type
    implements IReset :: Seal
        procedure, pass :: level => seal_level
        procedure, pass :: reset => seal_reset
        procedure, pass(self) :: restart => seal_restart
    end implements
    type :: Holder
        class(IReset), allocatable :: owned
    end type
contains
    integer function gauge_level(self)
        class(Gauge), intent(in) :: self
        gauge_level = self%n
    end function
    subroutine check_entry(self)
        class(Gauge), target, intent(in) :: self
        type(c_ptr) :: actual
        actual = c_loc(self%n)
        if (c_associated(expected)) then
            if (.not. c_associated(actual, expected)) error stop 90
        end if
        if (self%n /= 7 .or. allocated(self%data)) error stop 91
        entries = entries + 1
    end subroutine
    subroutine gauge_reset(self, start)
        class(Gauge), target, intent(out) :: self
        integer, intent(in) :: start
        call check_entry(self)
        self%n = start
        self%scratch = -start
        allocate(self%data(start))
    end subroutine
    integer function gauge_restart(start, self)
        integer, intent(in) :: start
        class(Gauge), target, intent(out) :: self
        call check_entry(self)
        self%n = start + 100
        allocate(self%data(1))
        gauge_restart = self%n
    end function
    subroutine gauge_final(self)
        type(Gauge), intent(inout) :: self
        finals = finals + 1
        if (allocated(self%data)) finals_with_data = finals_with_data + 1
    end subroutine
    integer function seal_level(self)
        type(Seal), intent(in) :: self
        seal_level = self%n
    end function
    subroutine seal_reset(self, start)
        type(Seal), intent(out) :: self
        integer, intent(in) :: start
        if (self%n /= 3 .or. allocated(self%data)) error stop 93
        entries = entries + 1
        self%n = start
        allocate(self%data(start))
    end subroutine
    integer function seal_restart(start, self)
        integer, intent(in) :: start
        type(Seal), intent(out) :: self
        if (self%n /= 3 .or. allocated(self%data)) error stop 94
        entries = entries + 1
        self%n = start + 100
        seal_restart = self%n
    end function
    subroutine reset_view(x, start)
        class(IReset), intent(inout) :: x
        integer, intent(in) :: start
        call x%reset(start=start)
    end subroutine
    integer function restart_view(x, start)
        class(IReset), intent(inout) :: x
        integer, intent(in) :: start
        restart_view = x%restart(start)
    end function
    subroutine reset_generic{IReset :: T}(x, start)
        type(T), intent(inout) :: x
        integer, intent(in) :: start
        call x%reset(start)
        if (x%level() /= start) error stop 95
    end subroutine
    ! Counts since the last call, then the state of a Gauge after reset(n).
    subroutine expect(f, fd, e)
        integer, intent(in) :: f, fd, e
        if (finals /= f .or. finals_with_data /= fd .or. entries /= e) then
            print '(a,3i4)', 'counts:', finals, finals_with_data, entries
            error stop 96
        end if
        finals = 0
        finals_with_data = 0
        entries = 0
    end subroutine
    subroutine expect_gauge(g, n)
        type(Gauge), intent(in) :: g
        integer, intent(in) :: n
        if (g%n /= n .or. .not. allocated(g%data)) error stop 97
        if (size(g%data) /= n) error stop 98
    end subroutine
end module

program traits_receiver_intent_03
    use iso_c_binding, only: c_loc, c_null_ptr
    use traits_receiver_intent_03_m
    implicit none
    type(Gauge), target :: g
    type(Seal) :: s
    type(Holder), target :: h
    class(IReset), allocatable, target :: owner
    class(IReset), pointer :: p
    integer :: k

    ! Direct call: one finalization that sees the old data, then the body.
    g%n = 1
    g%scratch = 1
    allocate(g%data(4))
    expected = c_loc(g%n)
    call g%reset(5)
    call expect(1, 1, 1)
    call expect_gauge(g, 5)
    ! An intent(inout) view borrowed from the variable.
    call reset_view(g, 6)
    call expect(1, 1, 1)
    call expect_gauge(g, 6)
    ! A static generic call.
    call reset_generic(g, 2)
    call expect(1, 1, 1)
    call expect_gauge(g, 2)
    ! An allocatable owner keeps its allocation and dynamic type.
    allocate(Gauge :: owner)
    select type (concrete => owner)
    type is (Gauge)
        expected = c_loc(concrete%n)
    class default
        error stop 2
    end select
    call owner%reset(4)
    call expect(1, 0, 1)
    if (.not. allocated(owner)) error stop 3
    select type (concrete => owner)
    type is (Gauge)
        if (concrete%n /= 4 .or. size(concrete%data) /= 4) error stop 4
    class default
        error stop 5
    end select
    call reset_view(owner, 2)
    call expect(1, 1, 1)
    if (owner%level() /= 2) error stop 6
    ! An owner component.
    allocate(Gauge :: h%owned)
    select type (concrete => h%owned)
    type is (Gauge)
        expected = c_loc(concrete%n)
    class default
        error stop 7
    end select
    call h%owned%reset(3)
    call expect(1, 0, 1)
    if (h%owned%level() /= 3) error stop 8
    ! A pointer to the variable.
    p => g
    expected = c_loc(g%n)
    call p%reset(1)
    call expect(1, 1, 1)
    call expect_gauge(g, 1)
    ! A function with an intent(out) receiver.
    k = g%restart(5)
    call expect(1, 1, 1)
    if (k /= 105 .or. g%n /= 105 .or. size(g%data) /= 1) error stop 9
    k = restart_view(g, 6)
    call expect(1, 1, 1)
    if (k /= 106 .or. g%level() /= 106) error stop 10
    ! A sealed type's TYPE passed object, without a finalizer.
    expected = c_null_ptr
    s%n = 9
    allocate(s%data(2))
    call s%reset(4)
    call expect(0, 0, 1)
    if (s%n /= 4 .or. size(s%data) /= 4) error stop 11
    call reset_view(s, 5)
    call expect(0, 0, 1)
    if (s%level() /= 5 .or. size(s%data) /= 5) error stop 12
    k = restart_view(s, 1)
    call expect(0, 0, 1)
    if (k /= 101 .or. s%n /= 101 .or. allocated(s%data)) error stop 13
    call reset_generic(s, 3)
    call expect(0, 0, 1)
    print '(a)', 'traits_receiver_intent_03 ok'
end program

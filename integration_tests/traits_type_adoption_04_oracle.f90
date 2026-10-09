module traits_type_adoption_04_oracle_m
    implicit none
    integer :: final_count = 0, final_sum = 0
    integer :: receiver_finals = 0

    type :: Marker
        integer :: stamp = 0
    contains
        final :: finish_marker
    end type

    type :: Parent
        integer :: n = 17
    contains
        procedure :: value => parent_value
        procedure, pass(self) :: named => parent_named
        procedure, pass(self) :: add => parent_add
    end type

    type, extends(Parent) :: Open
    contains
        procedure :: value => open_value
    end type

    type, extends(Parent) :: Closed
        integer :: bias = 2
    contains
        procedure :: value => closed_value
        procedure, pass(self) :: named => closed_named
        procedure, pass(self) :: add => closed_add
        final :: finish_closed
    end type
contains
    subroutine finish_marker(self)
        type(Marker), intent(inout) :: self
        final_count = final_count + 1
        final_sum = final_sum + self%stamp
    end subroutine

    subroutine finish_closed(self)
        type(Closed), intent(inout) :: self
        receiver_finals = receiver_finals + 1
    end subroutine

    integer function parent_value(self) result(n)
        class(Parent), intent(in) :: self
        n = self%n
    end function

    integer function open_value(self) result(n)
        class(Open), intent(in) :: self
        n = self%n + 1
    end function

    integer function closed_value(self) result(n)
        class(Closed), intent(in) :: self
        type(Marker) :: local
        local%stamp = 7
        n = self%n + self%bias
    end function

    pure integer function parent_named(scale, self, offset) result(n)
        integer, intent(in) :: scale
        class(Parent), intent(in) :: self
        integer, optional, intent(in) :: offset
        n = scale * self%n
        if (present(offset)) n = n + offset
    end function

    pure integer function closed_named(scale, self, offset) result(n)
        integer, intent(in) :: scale
        class(Closed), intent(in) :: self
        integer, optional, intent(in) :: offset
        n = scale * self%n + self%bias
        if (present(offset)) n = n + offset
    end function

    subroutine parent_add(step, self)
        integer, intent(in) :: step
        class(Parent), intent(inout) :: self
        self%n = self%n + step
    end subroutine

    subroutine closed_add(step, self)
        integer, intent(in) :: step
        class(Closed), intent(inout) :: self
        self%n = self%n + step + self%bias
    end subroutine

    integer function read_concrete(item) result(n)
        type(Closed), intent(in) :: item
        n = item%value()
    end function

    integer function read_runtime(item) result(n)
        class(Parent), intent(in) :: item
        n = item%value()
    end function

    pure integer function via_named(self, with_offset) result(n)
        class(Parent), intent(in) :: self
        logical, intent(in) :: with_offset
        if (with_offset) then
            n = self%named(offset=3, scale=2)
        else
            n = self%named(scale=2)
        end if
    end function

    subroutine via_add(self)
        class(Parent), intent(inout) :: self
        call self%add(step=5)
    end subroutine
end module

program traits_type_adoption_04_oracle
    use traits_type_adoption_04_oracle_m
    implicit none
    type(Open), target :: ordinary
    type(Closed), target :: object
    class(Closed), pointer :: exact
    class(Parent), pointer :: ancestor
    integer :: direct, through_exact, through_parent, with_offset, without_offset

    exact => object
    ancestor => object
    direct = object%value()
    through_exact = exact%value()
    if (direct /= 19 .or. through_exact /= 19) error stop 1
    if (final_count /= 2 .or. final_sum /= 14) error stop 2
    if (read_runtime(ordinary) /= 18) error stop 3

    through_parent = read_runtime(ancestor)
    if (through_parent /= 19) error stop 4
    if (final_count /= 3 .or. final_sum /= 21) error stop 5
    if (read_concrete(object) /= 19) error stop 6
    if (read_runtime(object) /= 19) error stop 7
    if (final_count /= 5 .or. final_sum /= 35) error stop 8

    with_offset = via_named(ancestor, .true.)
    without_offset = via_named(ancestor, .false.)
    if (with_offset /= 39 .or. without_offset /= 36) error stop 9
    call via_add(ancestor)
    if (object%n /= 24 .or. object%bias /= 2) error stop 10
    if (read_runtime(ancestor) /= 26) error stop 11
    if (final_count /= 6 .or. final_sum /= 42) error stop 12
    if (receiver_finals /= 0) error stop 13
    print *, "sealed ancestor dispatch: 19 39 36 26; finals: 6 42"
    nullify(exact, ancestor)
end program

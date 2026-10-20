module class_optional_05_m
    implicit none
    integer :: evaluations = 0
    type :: Value
        integer :: base = 7
    contains
        procedure, pass(self) :: get
        procedure, pass(self) :: values
        procedure :: first
        procedure, nopass :: bare
    end type
    type, extends(Value) :: Child
    end type
contains
    integer function get(increment, self)
        integer, optional, intent(in) :: increment
        class(Value), intent(in) :: self
        get = self%base
        if (present(increment)) get = get + increment
    end function
    function values(increment, self, n) result(output)
        integer, optional, intent(in) :: increment
        class(Value), intent(in) :: self
        integer, intent(in) :: n
        integer :: output(n)
        output = self%base
        if (present(increment)) output = output + increment
    end function
    integer function first(self, increment)
        class(Value), intent(in) :: self
        integer, optional, intent(in) :: increment
        first = self%base
        if (present(increment)) first = first + increment
    end function
    integer function bare(increment)
        integer, optional, intent(in) :: increment
        bare = 5
        if (present(increment)) bare = bare + increment
    end function
    integer function bump()
        evaluations = evaluations + 1
        bump = 3
    end function
end module

program class_optional_05
    use class_optional_05_m
    implicit none
    type(Value) :: object
    type(Child) :: inherited
    integer :: output(3)

    if (object%get() /= 7) error stop 1
    if (object%get(3) /= 10) error stop 2
    if (object%get(increment=4) /= 11) error stop 3
    if (object%get(bump()) /= 10 .or. evaluations /= 1) error stop 4
    if (object%get(object%get()) /= 14) error stop 5
    if (inherited%get() /= 7) error stop 6
    if (object%first() /= 7 .or. object%first(increment=2) /= 9) error stop 7
    if (object%bare() /= 5 .or. object%bare(increment=2) /= 7) error stop 8
    output = object%values(n=3)
    if (any(output /= 7)) error stop 9
    output = object%values(n=3, increment=2)
    if (any(output /= 9)) error stop 10
    output = object%values(4, n=3)
    if (any(output /= 11)) error stop 11
end program

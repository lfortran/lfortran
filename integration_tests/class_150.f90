program class_150
    implicit none

    type :: token
    end type

    class(token), pointer :: alias
    type(token), target :: value
    logical :: matches

    alias => value
    matches = same_type_as(alias, value)
    if (.not. matches) error stop
end program class_150

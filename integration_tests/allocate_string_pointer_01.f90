program allocate_string_pointer_01
    implicit none
    type record_t
        integer, pointer :: counts(:) => null()
        character(len=2), pointer :: short_names(:) => null()
        character(len=20), pointer :: long_names(:) => null()
    end type
    type(record_t) :: r
    character(len=2), pointer :: s(:)

    ! A pointer array of strings that is null must be allocatable
    nullify(s)
    allocate(s(4))
    s = "ab"
    if (s(3) /= "ab") error stop

    allocate(r%counts(3), r%short_names(3), r%long_names(3))
    r%counts = 1
    r%short_names = "xy"
    r%long_names = "xy_long"
    if (r%short_names(2) /= "xy" .or. r%long_names(3) /= "xy_long") error stop
    if (any(r%counts /= 1)) error stop
    print *, "ok"
end program

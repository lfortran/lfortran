program finalization_10
    ! A function referenced by a defined assignment in a procedure read from
    ! a module file is referenced once, and its result is finalized once,
    ! after the statement (F2018 7.5.6.3 p5).
    use finalization_10_module, only: counter_t, object_t, reset_counts, &
        ncreated, nassigned, nreleased
    implicit none
    type(object_t) :: object, started

    ! new_object: counter_t() (count 1), assigned to the result (count 2),
    ! finalized (count 1). The intrinsic assignment of object_t assigns the
    ! component by the defined assignment (count 2) and the result of
    ! new_object is finalized after the statement (count 1).
    object = object_t()
    print *, ncreated, nassigned, nreleased, object%counter%count
    if (ncreated /= 1) error stop 1
    if (nassigned /= 2) error stop 2
    if (nreleased /= 2) error stop 3
    if (.not. associated(object%counter%count)) error stop 4
    if (object%counter%count /= 1) error stop 5

    call reset_counts()
    call started%start_counter()
    print *, ncreated, nassigned, nreleased, started%counter%count
    if (ncreated /= 1) error stop 6
    if (nassigned /= 1) error stop 7
    if (nreleased /= 1) error stop 8
    if (.not. associated(started%counter%count)) error stop 9
    if (started%counter%count /= 1) error stop 10
end program

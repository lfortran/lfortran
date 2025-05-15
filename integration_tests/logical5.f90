program logical5
    implicit none
    logical, target :: val
    logical, pointer :: lval

    val = .true.
    lval => val

    if (.not. lval) error stop
end program logical5

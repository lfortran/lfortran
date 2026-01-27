program select_type_20
    implicit none

    logical, target :: l(3)
    class(*), pointer :: generic(:)

    l = [.true., .false., .true.]
    generic => l

    select type (x => generic)
    type is (logical)
        if (.not. x(1)) error stop
        if (x(2)) error stop
        if (count(x) /= 2) error stop
        x(2) = .true.
        if (count(l) /= 3) error stop
    class default
        error stop
    end select

    print *, "ok"
end program select_type_20

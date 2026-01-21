program select_type_18
    implicit none

    logical, target :: l(3)
    class(*), pointer :: generic(:)
    logical, pointer :: p(:)

    l = [.true., .false., .true.]
    generic => l

    select type (generic)
    type is (logical)
        p => generic
        if (count(p) /= 2) error stop
    class default
        error stop
    end select

    print *, "ok"
end program select_type_18

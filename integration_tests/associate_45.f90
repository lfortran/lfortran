program associate_45
    implicit none

    logical, target :: l(3)
    class(*), pointer :: generic(:)
    class(*), pointer :: generic2(:)

    l = [.true., .false., .true.]
    generic  => l
    generic  => l !intentionally associate twice to check memory leaks
    generic2 => l
    generic2 => l !intentionally associate twice to check memory leaks
    print *, associated(generic, generic2)
    if (.not. associated(generic, generic2)) error stop
end program
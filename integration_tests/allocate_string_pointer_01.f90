program allocate_string_pointer_01
    implicit none
    type w_t
        character(len=2), pointer :: c(:) => null()
    end type
    type(w_t) :: w
    character(len=2), pointer :: s(:) => null()
    character(len=2), pointer :: t(:)

    ! A pointer array of strings that is null must be allocatable
    allocate(s(3))
    if (size(s) /= 3) error stop
    deallocate(s)

    nullify(t)
    allocate(t(4))
    t = "ab"
    if (t(3) /= "ab") error stop
    deallocate(t)

    allocate(w%c(3))
    w%c = "xy"
    if (w%c(2) /= "xy") error stop
    deallocate(w%c)
    print *, "ok"
end program

program select_type_57
    implicit none
    character(len=5) :: s
    character(len=6), target :: t
    class(*), allocatable :: a
    class(*), pointer :: p

    s = "xxxxx"
    call set_value(s, "hi")
    if (s /= "hi   ") error stop
    if (len(s) /= 5) error stop

    s = "xxxxx"
    call set_value(s, "longer than five")
    if (s /= "longe") error stop

    allocate(a, source="abcd")
    select type (a)
    type is (character(len=*))
        a = "z"
        if (len(a) /= 4) error stop
        if (a /= "z   ") error stop
    end select

    t = "yyyyyy"
    p => t
    select type (v => p)
    type is (character(len=*))
        v = "ab"
        if (len(v) /= 6) error stop
    end select
    if (t /= "ab    ") error stop
    print *, s, t
contains
    subroutine set_value(value, new_value)
        class(*), intent(inout) :: value
        character(len=*), intent(in) :: new_value
        select type (value)
        type is (character(len=*))
            value = new_value
            if (len(value) /= 5) error stop
        end select
    end subroutine
end program

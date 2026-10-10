module select_type_54_mod
    implicit none
contains
    subroutine get_value(value, default_value)
        class(*), intent(inout) :: value
        class(*), intent(in) :: default_value
        character(len=10) :: t
        select type (value)
        type is (character(len=*))
            select type (default_value)
            type is (character(len=*))
                value = default_value
                t = default_value
                if (t /= "hello") error stop
                if (len(default_value) /= 5) error stop
                if (default_value // "!" /= "hello!") error stop
            end select
        end select
    end subroutine
end module

program select_type_54
    use select_type_54_mod
    implicit none
    character(len=5) :: s
    s = "xxxxx"
    call get_value(s, "hello")
    if (s /= "hello") error stop
    print *, s
end program

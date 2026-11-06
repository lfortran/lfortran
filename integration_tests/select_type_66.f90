module select_type_66_mod
    implicit none
contains
    subroutine set_default(value, default_value)
        class(*), intent(inout) :: value
        class(*), intent(in) :: default_value
        select type (value)
        type is (character(len=*))
            select type (default_value)
            type is (character(len=*))
                value = default_value
            end select
        end select
    end subroutine

    subroutine set_long(v)
        class(*), intent(inout) :: v
        select type (v)
        type is (character(len=*))
            v = "abcdefghijklmnopqrstu"
        end select
    end subroutine
end module

program select_type_66
    use select_type_66_mod
    implicit none
    character(len=16) :: r
    character(len=:), allocatable :: a
    r = repeat("x", 16)
    call set_default(r, "williamson")
    if (len(r) /= 16) error stop
    if (r /= "williamson") error stop
    if (r(11:16) /= "      ") error stop
    call set_long(r)
    if (r /= "abcdefghijklmnop") error stop
    a = "xyz"
    call set_default(a, "q")
    if (len(a) /= 3) error stop
    if (a /= "q  ") error stop
end program

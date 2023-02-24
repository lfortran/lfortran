module derived_types_14_module1
    use derived_types_14_module
    implicit none
    contains
        !> Determine TOML value type
        function toml_get_value_type(raw) result(vtype)

            !> Raw representation of TOML string
            character(len=*), intent(in) :: raw

            !> Value type
            integer :: vtype

            if (check(raw)) then
            vtype = toml_type%string
            return
            end if
            if (check(raw)) then
            vtype = toml_type%boolean
            return
            end if
            if (check(raw)) then
            vtype = toml_type%int
            return
            end if
            if (check(raw)) then
            vtype = toml_type%float
            return
            end if
            if (check(raw)) then
            vtype = toml_type%datetime
            return
            end if
            vtype = toml_type%invalid

        end function

        function check(raw) result(res)

            !> Raw representation of TOML string
            character(len=*), intent(in) :: raw

            !> Value type
            logical :: res
            res = .true.

        end function

end module derived_types_14_module1

program main
    implicit none

    print *, "running derived_types_14 main program"

end program main

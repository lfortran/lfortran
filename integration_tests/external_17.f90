program external_17
    use iso_c_binding, only: c_int
    implicit none

    interface
        subroutine c_external(value) bind(c)
            import c_int
            integer(c_int), intent(out) :: value
        end subroutine
    end interface

    integer(c_int) :: c_value, legacy_value

    call c_external(c_value)
    call legacy_external(legacy_value)
    if (c_value /= 1_c_int) error stop
    if (legacy_value /= 2_c_int) error stop
end program

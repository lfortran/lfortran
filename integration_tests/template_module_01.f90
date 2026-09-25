module template_module_01_m
    implicit none

    requirement r {t}
        deferred type :: t
    end requirement

    template tmpl {t}
        require r {t}
    contains
        subroutine inc(i, x)
            integer, intent(inout) :: i
            type(t), intent(in) :: x
            i = i + 1
        end subroutine

        function identity(x) result(y)
            type(t), intent(in) :: x
            type(t) :: y
            y = x
        end function
    end template

    instantiate tmpl {integer}
end module

module template_module_01_import_m
    use template_module_01_m, only: tmpl
    implicit none
    instantiate tmpl {real}, only: inc_real => inc, identity_real => identity
end module

program template_module_01
    use template_module_01_m, only: tmpl, inc, identity
    use template_module_01_import_m, only: inc_real, identity_real
    implicit none
    instantiate tmpl {integer(8)}, only: inc_i8 => inc, identity_i8 => identity
    integer :: local_count, imported_count, program_count

    local_count = 0
    imported_count = 0
    program_count = 0
    call inc(local_count, 1)
    call inc_real(imported_count, 2.0)
    call inc_i8(program_count, 3_8)
    print *, local_count, imported_count, program_count
    if (local_count /= 1) error stop 1
    if (imported_count /= 1) error stop 2
    if (program_count /= 1) error stop 3
    if (identity(42) /= 42) error stop 4
    if (abs(identity_real(4.5) - 4.5) > 1.e-6) error stop 5
    if (identity_i8(1234567890123_8) /= 1234567890123_8) error stop 6
end program

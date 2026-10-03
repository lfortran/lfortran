module optional_procedure_dummy_01_m
    implicit none
contains
    subroutine rd(x, get_ptr)
        integer, intent(inout) :: x
        interface
            subroutine get_ptr(ptr)
                integer, intent(out) :: ptr
            end subroutine
        end interface
        optional :: get_ptr
        if (present(get_ptr)) then
            call get_ptr(x)
        else
            x = 1
        end if
    end subroutine
end module optional_procedure_dummy_01_m

program optional_procedure_dummy_01
    use optional_procedure_dummy_01_m
    implicit none
    integer :: x
    call rd(x)
    if (x /= 1) error stop
end program optional_procedure_dummy_01

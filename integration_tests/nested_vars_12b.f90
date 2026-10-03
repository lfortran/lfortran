module nested_vars_12_mod_b
    implicit none
contains
    subroutine process(x)
        integer, intent(inout) :: x
        call inner()
    contains
        subroutine inner()
            x = x * 2
        end subroutine inner
    end subroutine process
end module nested_vars_12_mod_b

module associate_57_mod
    implicit none
    real, target :: tgt(2:4) = [20., 30., 40.]
    integer, target :: scalar_tgt = 7
contains
    function f_ptr() result(p)
        real, pointer :: p(:)
        p => tgt
    end function

    function f_scalar_ptr() result(p)
        integer, pointer :: p
        p => scalar_tgt
    end function
end module

program associate_57
    use associate_57_mod
    implicit none

    associate (fp => f_ptr())
        if (size(fp) /= 3) error stop
        if (abs(fp(lbound(fp,1)) - 20.) > 1e-6) error stop
        if (abs(sum(fp) - 90.) > 1e-6) error stop
    end associate

    associate (sp => f_scalar_ptr())
        if (sp /= 7) error stop
    end associate

    call contained_case()

contains

    subroutine contained_case()
        associate (fp => f_ptr())
            if (size(fp) /= 3) error stop
            if (abs(sum(fp) - 90.) > 1e-6) error stop
        end associate
    end subroutine

end program

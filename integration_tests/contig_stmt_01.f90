program contig_stmt_01
    implicit none

    character(len=1) :: nml(1)
    real(8) :: nmv(1)

    call eval_equation("x", nml, nmv)

contains

    subroutine eval_equation(ln, nml, nmv)
        character(*), intent(in) :: ln
        character(*), dimension(:), intent(in) :: nml
        real(8), dimension(:), intent(inout) :: nmv
        contiguous :: nml, nmv

        if (ln /= "x") error stop
        nmv(1) = 42.0d0
        print *, "PASS"
    end subroutine eval_equation

end program contig_stmt_01

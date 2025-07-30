module bspline_sub_module
    contains
    subroutine check_inputs(nx)
        implicit none
        integer(4),intent(in),optional :: nx
        call check(nx)
        contains
            subroutine check(n)
                implicit none
                integer(4),intent(in),optional :: n     
                print *, present(n)
                if (present(n) .neqv. .true.) error stop
            end subroutine check
    end subroutine check_inputs
end module bspline_sub_module

program intrinsics_392
    use bspline_sub_module
    implicit none
    integer(4) :: nx
    call check_inputs(nx)
end program intrinsics_392

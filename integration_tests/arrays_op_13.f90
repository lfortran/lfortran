module arrays_op_13
    use, intrinsic :: iso_c_binding, only : c_char, c_null_char
    use iso_fortran_env, only: int64
    private
    public :: fpm_model_t, package_t, srcfile_t

    !> Source type unknown
    integer, parameter :: FPM_UNIT_UNKNOWN = -1

    type srcfile_t
        !> Type of source unit
        integer :: unit_type = FPM_UNIT_UNKNOWN

    end type srcfile_t

    type package_t

        !> Array of sources
        type(srcfile_t), allocatable :: sources(:)

    end type package_t

    type :: fpm_model_t

        !> Array of packages (including the root package)
        type(package_t), allocatable :: packages(:)

    end type fpm_model_t

contains

    subroutine build_target_list(model)

        type(fpm_model_t), intent(inout), target :: model

        associate(sources=>model%packages(0)%sources)

        end associate


    end subroutine build_target_list

end module arrays_op_13

program arrays_op_13_main
    implicit none
    print *, "working ok"
end program arrays_op_13_main

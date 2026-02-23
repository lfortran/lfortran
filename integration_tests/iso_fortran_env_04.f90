program iso_fortran_env_04
    use, intrinsic :: iso_fortran_env, only: input_unit, output_unit, error_unit, &
        iostat_end, iostat_eor, &
        numeric_storage_size, character_storage_size, file_storage_size
    implicit none

    ! F90: I/O unit numbers
    if (input_unit /= 5) error stop
    if (output_unit /= 6) error stop
    if (error_unit /= 0) error stop

    ! F90: I/O status constants
    if (iostat_end /= -1) error stop
    if (iostat_eor /= -2) error stop

    ! F2003: Storage sizes (in bits)
    if (file_storage_size /= 8) error stop
    if (numeric_storage_size /= 32) error stop
    if (character_storage_size /= 8) error stop

    print *, "iso_fortran_env_04: all F2003 constants OK"
end program iso_fortran_env_04

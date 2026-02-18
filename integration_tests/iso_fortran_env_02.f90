program iso_fortran_env_02
    use iso_fortran_env, only: character_storage_size, file_storage_size, &
        numeric_storage_size, iostat_eor, iostat_end, &
        integer_kinds, real_kinds, character_kinds, logical_kinds, &
        int8, int16, int32, int64, real32, real64, &
        input_unit, output_unit, error_unit, &
        iostat_inquire_internal_unit, &
        stat_unlocked, stat_locked, stat_locked_other_image, &
        stat_stopped_image, &
        compiler_version, compiler_options
    implicit none

    ! F2003: Storage sizes
    if (numeric_storage_size /= 32) error stop
    if (character_storage_size /= 8) error stop
    if (file_storage_size /= 8) error stop

    ! F2003: I/O status constants
    if (iostat_eor /= -2) error stop
    if (iostat_end /= -1) error stop

    ! F2008: I/O status constants
    ! iostat_inquire_internal_unit must be negative and different from
    ! iostat_end and iostat_eor
    if (iostat_inquire_internal_unit >= 0) error stop
    if (iostat_inquire_internal_unit == iostat_end) error stop
    if (iostat_inquire_internal_unit == iostat_eor) error stop

    ! F2003: I/O units
    if (input_unit /= 5) error stop
    if (output_unit /= 6) error stop
    if (error_unit /= 0) error stop

    ! F2003: Kind type parameters
    if (int8 /= 1) error stop
    if (int16 /= 2) error stop
    if (int32 /= 4) error stop
    if (int64 /= 8) error stop
    if (real32 /= 4) error stop
    if (real64 /= 8) error stop

    ! F2008/F2018: Kind arrays
    if (size(integer_kinds) /= 4) error stop
    if (integer_kinds(1) /= 1) error stop
    if (integer_kinds(2) /= 2) error stop
    if (integer_kinds(3) /= 4) error stop
    if (integer_kinds(4) /= 8) error stop

    if (size(real_kinds) /= 2) error stop
    if (real_kinds(1) /= 4) error stop
    if (real_kinds(2) /= 8) error stop

    if (size(character_kinds) /= 1) error stop
    if (character_kinds(1) /= 1) error stop

    if (size(logical_kinds) /= 4) error stop
    if (logical_kinds(1) /= 1) error stop
    if (logical_kinds(2) /= 2) error stop
    if (logical_kinds(3) /= 4) error stop
    if (logical_kinds(4) /= 8) error stop

    ! F2008: Coarray stat constants
    ! These just need to be defined and have distinct values from each other
    if (stat_unlocked == stat_locked) error stop
    if (stat_locked == stat_locked_other_image) error stop

    ! compiler_version and compiler_options
    if (len(compiler_version()) == 0) error stop
    if (len(compiler_options()) < 0) error stop
end program iso_fortran_env_02

program iso_fortran_env_01
    use iso_fortran_env, only: numeric_storage_size, character_storage_size, iostat_eor
    print *, "numeric_storage_size: ", numeric_storage_size
    if (numeric_storage_size /= 32) error stop

    print *, "character_storage_size: ", character_storage_size
    if (character_storage_size /= 8) error stop

    print *, "iostat_eor: ", iostat_eor
    if (iostat_eor /= -2) error stop
end program

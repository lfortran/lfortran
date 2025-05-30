program iso_fortran_env_02
    use iso_fortran_env, only: character_storage_size
    print *, "character_storage_size: ", character_storage_size
    if (character_storage_size /= 8) error stop
end program

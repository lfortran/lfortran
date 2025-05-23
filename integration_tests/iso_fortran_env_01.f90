program main
    use iso_fortran_env, only: numeric_storage_size
    print *, "numeric_storage_size: ", numeric_storage_size
    if (numeric_storage_size /= 32) error stop
end program

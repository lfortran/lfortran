program flush_03
    use iso_fortran_env
    implicit none
    FLUSH(INPUT_UNIT)
    FLUSH(OUTPUT_UNIT)
    FLUSH(ERROR_UNIT)
end program flush_03

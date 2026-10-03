program flush_04
    use iso_fortran_env, only: output_unit, error_unit
    implicit none
    flush output_unit
    flush error_unit
    flush 6
end program flush_04

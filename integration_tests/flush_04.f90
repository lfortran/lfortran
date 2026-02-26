program flush_04
    use iso_fortran_env, only: output_unit, error_unit
    implicit none
    integer :: u
    u = output_unit
    write(u, *) "hello"
    flush u
    flush output_unit
    flush error_unit
    print *, "ok"
end program

program separate_compilatio_09
    use mpi_separate_compilation_09b

    integer :: comm_f
    comm_f = 1000
    print *, handle_mpi_comm_f2c(comm_f)
    if ( handle_mpi_comm_f2c(comm_f) /= 412 ) error stop
end program separate_compilatio_09


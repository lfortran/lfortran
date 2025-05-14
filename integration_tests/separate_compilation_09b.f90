module mpi_separate_compilation_09b
    implicit none

    integer, parameter :: MPI_COMM_WORLD = -1000

    contains
    integer(kind=4) function handle_mpi_comm_f2c(comm_f) result(c_comm)
        use mpi_c_bindings_separate_compilation_09a, only: c_mpi_comm_world
        integer, intent(in) :: comm_f
        c_comm = c_mpi_comm_world
    end function handle_mpi_comm_f2c

end module mpi_separate_compilation_09b

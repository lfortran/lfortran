module separate_compilation_07a_mpi_c_bindings
    implicit none

    interface
        subroutine c_mpi_init(ierr) bind(C, name="mpi_init_wrapper")
            use iso_c_binding, only: c_int
            integer(c_int), intent(out) :: ierr
        end subroutine c_mpi_init
    end interface
end module separate_compilation_07a_mpi_c_bindings


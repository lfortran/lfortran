module separate_compilation_07b_mpi
    implicit none
    interface MPI_Init
        module procedure MPI_Init_proc
    end interface MPI_Init

   contains

    subroutine MPI_Init_proc(ierr)
        use separate_compilation_07a_mpi_c_bindings, only: c_mpi_init
        use iso_c_binding, only : c_int
        integer, optional, intent(out) :: ierr
        integer :: local_ierr
        if (present(ierr)) then
            call c_mpi_init(ierr)
        else
            call c_mpi_init(local_ierr)
            if (local_ierr /= 5) then
                print *, "MPI_Init failed with error code: ", local_ierr
            end if
        end if
    end subroutine

end module separate_compilation_07b_mpi


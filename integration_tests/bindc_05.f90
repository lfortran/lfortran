module mpi_barr
    contains

    function c_mpi_comm_f2c(comm_f) bind(C, name="MPI_Comm_f2c")
        use iso_c_binding, only: c_int, c_ptr
        integer(c_int), value :: comm_f
        type(c_ptr) :: c_mpi_comm_f2c  ! MPI_Comm as pointer
    end function c_mpi_comm_f2c

    subroutine MPI_Barrier(comm, ierror)
        use iso_c_binding, only: c_int, c_ptr
        integer, intent(in) :: comm
        integer, intent(out), optional :: ierror
        type(c_ptr) :: c_comm

        c_comm = c_mpi_comm_f2c(comm)
    end subroutine MPI_Barrier
end module



program bindc_05
    use mpi_barr
    implicit none
    integer, parameter :: MPI_COMM_WORLD = 0
    integer :: ierr
    call MPI_Barrier(MPI_COMM_WORLD, ierr)
end program
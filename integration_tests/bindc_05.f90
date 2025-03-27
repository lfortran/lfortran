module mpi
    use iso_c_binding, only: c_int, c_ptr, c_f_pointer, c_null_ptr, c_associated
    implicit none

    interface
        function ax(comm_f) bind(C, name="ax")
            import :: c_int, c_ptr
            integer(c_int), value :: comm_f
            type(c_ptr) :: ax
        end function ax
    end interface

contains

    subroutine MPI_Barrier(comm, ierror)
        integer, intent(in) :: comm
        integer, intent(out), optional :: ierror
        type(c_ptr) :: c_comm
        integer, pointer :: fortran_ptr

        ! Call the C function
        c_comm = ax(comm)
        if (.not. c_associated(c_comm)) then
            print *, "Error: Null pointer returned from C function"
            if (present(ierror)) ierror = 1
            return
        end if

        ! Convert C pointer to Fortran pointer
        call c_f_pointer(c_comm, fortran_ptr)
        print *, "Fortran received value:", fortran_ptr

        ! Verify the value matches what was passed
        if (fortran_ptr /= comm) then
            print *, "Error: Value mismatch"
            if (present(ierror)) ierror = 1
        else
            if (present(ierror)) ierror = 0
        end if
    end subroutine MPI_Barrier
end module mpi

program bindc_05
    use mpi
    implicit none
    integer, parameter :: MPI_COMM_WORLD = 42  ! Non-zero value
    integer :: ierr

    call MPI_Barrier(MPI_COMM_WORLD, ierr)
    if (ierr == 0) then
        print *, "Test passed successfully"
    else
        print *, "Test failed"
    end if
end program bindc_05
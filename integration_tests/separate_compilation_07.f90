program separate_compilation_07
    use separate_compilation_07b_mpi
    integer :: ierr
    call MPI_Init(ierr)
    print *, ierr
end program

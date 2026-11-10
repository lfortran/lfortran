program assumed_rank_28
    ! Passing a scalar named constant to an assumed-rank dummy (#5685)
    implicit none
    integer :: last_rank = -1
    integer :: last_size = -1
    call magnetic_energy()
contains

    subroutine MPI_Allreduce(sendbuf)
        real(8), dimension(..), intent(in) :: sendbuf
        last_rank = rank(sendbuf)
        last_size = size(sendbuf)
    end subroutine

    subroutine magnetic_energy
        implicit none
        real(8), parameter :: MPI_IN_PLACE = 1
        integer, parameter :: MPI_SUM = 1
        real(8) :: v(3), m(2, 3)
        call MPI_Allreduce(MPI_IN_PLACE)
        if (last_rank /= 0) error stop
        if (last_size /= 1) error stop
        v = 0
        m = 0
        call MPI_Allreduce(v)
        if (last_rank /= 1) error stop
        if (last_size /= 3) error stop
        call MPI_Allreduce(m)
        if (last_rank /= 2) error stop
        if (last_size /= 6) error stop
        print *, last_rank, last_size
    end subroutine
end program

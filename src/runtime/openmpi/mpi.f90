! https://docs.open-mpi.org/en/v5.0.0/man-openmpi/man3
! NOTE: this document currently only declares openmpi
! subroutines or constants which are used in the repository
! [POT3D](https://github.com/predsci/POT3D)
module mpi
    implicit none

    type MPI_Datatype
    end type

    type MPI_Comm
    end type

    type MPI_info
    end type

    type MPI_Op
    end type

    type MPI_Request
    end type

    type MPI_Status
    end type

    contains

    subroutine MPI_Bcast(buffer, count, datatype, root, comm, ierror)
        ! TYPE(*), DIMENSION(..) :: buffer
        TYPE(INTEGER), DIMENSION(..) :: buffer
        INTEGER, INTENT(IN) :: count, root
        TYPE(MPI_Datatype), INTENT(IN) :: datatype
        TYPE(MPI_Comm), INTENT(IN) :: comm
        INTEGER, OPTIONAL, INTENT(OUT) :: ierror
    end subroutine MPI_Bcast

    subroutine MPI_Barrier(comm, ierror)
        TYPE(MPI_Comm), INTENT(IN) :: comm
        INTEGER, OPTIONAL, INTENT(OUT) :: ierror
    end subroutine MPI_Barrier

    subroutine MPI_Finalize(ierror)
        INTEGER, OPTIONAL, INTENT(OUT) :: ierror
    end subroutine MPI_Finalize

    subroutine MPI_Init_thread(required, provided, ierror)
        INTEGER, INTENT(IN) :: required
        INTEGER, INTENT(OUT) :: provided
        INTEGER, OPTIONAL, INTENT(OUT) :: ierror
    end subroutine MPI_Init_thread

    subroutine MPI_Comm_size(comm, size, ierror)
        TYPE(MPI_Comm), INTENT(IN) :: comm
        INTEGER, INTENT(OUT) :: size
        INTEGER, OPTIONAL, INTENT(OUT) :: ierror
    end subroutine MPI_Comm_size

    subroutine MPI_Comm_rank(comm, rank, ierror)
        TYPE(MPI_Comm), INTENT(IN) :: comm
        INTEGER, INTENT(OUT) :: rank
        INTEGER, OPTIONAL, INTENT(OUT) :: ierror
    end subroutine MPI_Comm_rank

    subroutine MPI_Comm_split_type(comm, split_type, key, info, newcomm, ierror)
        TYPE(MPI_Comm), INTENT(IN) :: comm
        INTEGER, INTENT(IN) :: split_type, key
        TYPE(MPI_Info), INTENT(IN) :: info
        TYPE(MPI_Comm), INTENT(OUT) :: newcomm
        INTEGER, OPTIONAL, INTENT(OUT) :: ierror
    end subroutine MPI_Comm_split_type

    subroutine MPI_Cart_Create(comm_old, ndims, dims, periods, reorder, comm_cart, ierror)
        TYPE(MPI_Comm), INTENT(IN) :: comm_old
        INTEGER, INTENT(IN) :: ndims, dims(ndims)
        LOGICAL, INTENT(IN) :: periods(ndims), reorder
        TYPE(MPI_Comm), INTENT(OUT) :: comm_cart
        INTEGER, OPTIONAL, INTENT(OUT) :: ierror
    end subroutine MPI_Cart_Create

    subroutine MPI_Cart_Coords(comm, rank, maxdims, coords, ierror)
        TYPE(MPI_Comm), INTENT(IN) :: comm
        INTEGER, INTENT(IN) :: rank, maxdims
        INTEGER, INTENT(OUT) :: coords(maxdims)
        INTEGER, OPTIONAL, INTENT(OUT) :: ierror
    end subroutine MPI_Cart_Coords

    subroutine MPI_Cart_shift(comm, direction, disp, rank_source, rank_dest, ierror)
        TYPE(MPI_Comm), INTENT(IN) :: comm
        INTEGER, INTENT(IN) :: direction, disp
        INTEGER, INTENT(OUT) :: rank_source, rank_dest
        INTEGER, OPTIONAL, INTENT(OUT) :: ierror
    end subroutine MPI_Cart_shift

    subroutine MPI_Cart_sub(comm, remain_dims, newcomm, ierror)
        TYPE(MPI_Comm), INTENT(IN) :: comm
        LOGICAL, INTENT(IN) :: remain_dims(*)
        TYPE(MPI_Comm), INTENT(OUT) :: newcomm
        INTEGER, OPTIONAL, INTENT(OUT) :: ierror
    end subroutine MPI_Cart_sub

    subroutine MPI_Allgather(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, ierror)
        ! TYPE(*), DIMENSION(..), INTENT(IN) :: sendbuf
        TYPE(INTEGER), DIMENSION(..), INTENT(IN) :: sendbuf
        ! TYPE(*), DIMENSION(..) :: recvbuf
        TYPE(INTEGER), DIMENSION(..) :: recvbuf
        INTEGER, INTENT(IN) :: sendcount, recvcount
        TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
        TYPE(MPI_Comm), INTENT(IN) :: comm
        INTEGER, OPTIONAL, INTENT(OUT) :: ierror
    end subroutine MPI_Allgather

    subroutine MPI_Allreduce(sendbuf, recvbuf, count, datatype, op, comm, ierror)
        ! TYPE(*), DIMENSION(..), INTENT(IN) :: sendbuf
        TYPE(INTEGER), DIMENSION(..), INTENT(in) :: sendbuf
        ! TYPE(*), DIMENSION(..) :: recvbuf
        TYPE(INTEGER), DIMENSION(..) :: recvbuf
        INTEGER, INTENT(IN) :: count
        TYPE(MPI_Datatype), INTENT(IN) :: datatype
        TYPE(MPI_Op), INTENT(IN) :: op
        TYPE(MPI_Comm), INTENT(IN) :: comm
        INTEGER, OPTIONAL, INTENT(OUT) :: ierror
    end subroutine MPI_Allreduce

    subroutine MPI_Isend(buf, count, datatype, dest, tag, comm, request, ierror)
        ! TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: buf
        TYPE(INTEGER), DIMENSION(..), INTENT(IN) :: buf
        INTEGER, INTENT(IN) :: count, dest, tag
        TYPE(MPI_Datatype), INTENT(IN) :: datatype
        TYPE(MPI_Comm), INTENT(IN) :: comm
        TYPE(MPI_Request), INTENT(OUT) :: request
        INTEGER, OPTIONAL, INTENT(OUT) :: ierror
    end subroutine MPI_Isend

    subroutine MPI_Irecv(buf, count, datatype, source, tag, comm, request, ierror)
        ! TYPE(*), DIMENSION(..), ASYNCHRONOUS :: buf
        TYPE(INTEGER), DIMENSION(..) :: buf
        INTEGER, INTENT(IN) :: count, source, tag
        TYPE(MPI_Datatype), INTENT(IN) :: datatype
        TYPE(MPI_Comm), INTENT(IN) :: comm
        TYPE(MPI_Request), INTENT(OUT) :: request
        INTEGER, OPTIONAL, INTENT(OUT) :: ierror
    end subroutine MPI_Irecv

    subroutine MPI_Waitall(count, array_of_requests, array_of_statuses, ierror)
        INTEGER, INTENT(IN) :: count
        TYPE(MPI_Request), INTENT(INOUT) :: array_of_requests(count)
        TYPE(MPI_Status) :: array_of_statuses(*)
        INTEGER, OPTIONAL, INTENT(OUT) :: ierror
    end subroutine MPI_Waitall

    subroutine MPI_Recv(buf, count, datatype, source, tag, comm, status, ierror)
        ! TYPE(*), DIMENSION(..) :: buf
        TYPE(INTEGER), DIMENSION(..) :: buf
        INTEGER, INTENT(IN) :: count, source, tag
        TYPE(MPI_Datatype), INTENT(IN) :: datatype
        TYPE(MPI_Comm), INTENT(IN) :: comm
        TYPE(MPI_Status) :: status
        INTEGER, OPTIONAL, INTENT(OUT) :: ierror
    end subroutine MPI_Recv

    subroutine MPI_Ssend(buf, count, datatype, dest, tag, comm, ierror)
        ! TYPE(*), DIMENSION(..), INTENT(IN) :: buf
        TYPE(INTEGER), DIMENSION(..), INTENT(IN) :: buf
        INTEGER, INTENT(IN) :: count, dest, tag
        TYPE(MPI_Datatype), INTENT(IN) :: datatype
        TYPE(MPI_Comm), INTENT(IN) :: comm
        INTEGER, OPTIONAL, INTENT(OUT) :: ierror
    end subroutine MPI_Ssend
end module mpi

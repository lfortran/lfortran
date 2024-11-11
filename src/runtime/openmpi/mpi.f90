module mpi
    use, intrinsic :: iso_c_binding
    implicit none

    type, bind(C) :: MPI_Datatype
        type(C_PTR) :: c_ptr
    end type MPI_Datatype

    type, bind(C) :: MPI_Comm
        type(C_PTR) :: c_ptr
    end type MPI_Comm

    type, bind(C) :: MPI_Info
        type(C_PTR) :: c_ptr
    end type MPI_Info

    type, bind(C) :: MPI_Op
        type(C_PTR) :: c_ptr
    end type MPI_Op

    type, bind(C) :: MPI_Request
        type(C_PTR) :: c_ptr
    end type MPI_Request

    type, bind(C) :: MPI_Status
        integer(C_INT) :: MPI_SOURCE, MPI_TAG, MPI_ERROR
    end type MPI_Status

    contains

    subroutine MPI_Bcast(buffer, count, datatype, root, comm, ierror) bind(C, name="MPI_Bcast")
        type(C_PTR), value :: buffer
        integer(C_INT), value :: count, root
        type(MPI_Datatype), value :: datatype
        type(MPI_Comm), value :: comm
        integer(C_INT), intent(out), optional :: ierror
    end subroutine MPI_Bcast

    subroutine MPI_Barrier(comm, ierror) bind(C, name="MPI_Barrier")
        type(MPI_Comm), value :: comm
        integer(C_INT), intent(out), optional :: ierror
    end subroutine MPI_Barrier

    subroutine MPI_Finalize(ierror) bind(C, name="MPI_Finalize")
        integer(C_INT), intent(out), optional :: ierror
    end subroutine MPI_Finalize

    subroutine MPI_Init_thread(required, provided, ierror) bind(C, name="MPI_Init_thread")
        integer(C_INT), value :: required
        integer(C_INT), intent(out) :: provided
        integer(C_INT), intent(out), optional :: ierror
    end subroutine MPI_Init_thread

    subroutine MPI_Comm_size(comm, size, ierror) bind(C, name="MPI_Comm_size")
        type(MPI_Comm), value :: comm
        integer(C_INT), intent(out) :: size
        integer(C_INT), intent(out), optional :: ierror
    end subroutine MPI_Comm_size

    subroutine MPI_Comm_rank(comm, rank, ierror) bind(C, name="MPI_Comm_rank")
        type(MPI_Comm), value :: comm
        integer(C_INT), intent(out) :: rank
        integer(C_INT), intent(out), optional :: ierror
    end subroutine MPI_Comm_rank

    subroutine MPI_Comm_split_type(comm, split_type, key, info, newcomm, ierror) bind(C, name="MPI_Comm_split_type")
        type(MPI_Comm), value :: comm
        integer(C_INT), value :: split_type, key
        type(MPI_Info), value :: info
        type(MPI_Comm), intent(out) :: newcomm
        integer(C_INT), intent(out), optional :: ierror
    end subroutine MPI_Comm_split_type

    subroutine MPI_Cart_Create(comm_old, ndims, dims, periods, reorder, comm_cart, ierror) bind(C, name="MPI_Cart_Create")
        implicit none
        type(MPI_Comm), value :: comm_old
        integer(C_INT), value :: ndims
        integer(C_INT), dimension(ndims) :: dims
        logical(c_bool), dimension(ndims) :: periods
        logical(c_bool), value :: reorder
        type(MPI_Comm), intent(out) :: comm_cart
        integer(C_INT), intent(out), optional :: ierror
    end subroutine MPI_Cart_Create

    subroutine MPI_Cart_Coords(comm, rank, maxdims, coords, ierror) bind(C, name="MPI_Cart_Coords")
        type(MPI_Comm), value :: comm
        integer(C_INT), value :: rank, maxdims
        integer(C_INT), dimension(maxdims), intent(out) :: coords
        integer(C_INT), intent(out), optional :: ierror
    end subroutine MPI_Cart_Coords

    subroutine MPI_Cart_shift(comm, direction, disp, rank_source, rank_dest, ierror) bind(C, name="MPI_Cart_shift")
        type(MPI_Comm), value :: comm
        integer(C_INT), value :: direction, disp
        integer(C_INT), intent(out) :: rank_source, rank_dest
        integer(C_INT), intent(out), optional :: ierror
    end subroutine MPI_Cart_shift

    subroutine MPI_Cart_sub(comm, remain_dims, newcomm, ierror) bind(C, name="MPI_Cart_sub")
        type(MPI_Comm), value :: comm
        logical(C_bool), dimension(:) :: remain_dims
        type(MPI_Comm), intent(out) :: newcomm
        integer(C_INT), intent(out), optional :: ierror
    end subroutine MPI_Cart_sub

    subroutine MPI_Allgather(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, ierror) bind(C, name="MPI_Allgather")
        type(C_PTR), value :: sendbuf, recvbuf
        integer(C_INT), value :: sendcount, recvcount
        type(MPI_Datatype), value :: sendtype, recvtype
        type(MPI_Comm), value :: comm
        integer(C_INT), intent(out), optional :: ierror
    end subroutine MPI_Allgather

    subroutine MPI_Allreduce(sendbuf, recvbuf, count, datatype, op, comm, ierror) bind(C, name="MPI_Allreduce")
        type(C_PTR), value :: sendbuf, recvbuf
        integer(C_INT), value :: count
        type(MPI_Datatype), value :: datatype
        type(MPI_Op), value :: op
        type(MPI_Comm), value :: comm
        integer(C_INT), intent(out), optional :: ierror
    end subroutine MPI_Allreduce

    subroutine MPI_Isend(buf, count, datatype, dest, tag, comm, request, ierror) bind(C, name="MPI_Isend")
        type(C_PTR), value :: buf
        integer(C_INT), value :: count, dest, tag
        type(MPI_Datatype), value :: datatype
        type(MPI_Comm), value :: comm
        type(MPI_Request), intent(out) :: request
        integer(C_INT), intent(out), optional :: ierror
    end subroutine MPI_Isend

    subroutine MPI_Irecv(buf, count, datatype, source, tag, comm, request, ierror) bind(C, name="MPI_Irecv")
        type(C_PTR), value :: buf
        integer(C_INT), value :: count, source, tag
        type(MPI_Datatype), value :: datatype
        type(MPI_Comm), value :: comm
        type(MPI_Request), intent(out) :: request
        integer(C_INT), intent(out), optional :: ierror
    end subroutine MPI_Irecv

    subroutine MPI_Waitall(count, array_of_requests, array_of_statuses, ierror) bind(C, name="MPI_Waitall")
        integer(C_INT), value :: count
        type(MPI_Request), dimension(count), intent(inout) :: array_of_requests
        type(MPI_Status), dimension(:), intent(out) :: array_of_statuses
        integer(C_INT), intent(out), optional :: ierror
    end subroutine MPI_Waitall

    subroutine MPI_Recv(buf, count, datatype, source, tag, comm, status, ierror) bind(C, name="MPI_Recv")
        type(C_PTR), value :: buf
        integer(C_INT), value :: count, source, tag
        type(MPI_Datatype), value :: datatype
        type(MPI_Comm), value :: comm
        type(MPI_Status), intent(out) :: status
        integer(C_INT), intent(out), optional :: ierror
    end subroutine MPI_Recv

    subroutine MPI_Ssend(buf, count, datatype, dest, tag, comm, ierror) bind(C, name="MPI_Ssend")
        type(C_PTR), value :: buf
        integer(C_INT), value :: count, dest, tag
        type(MPI_Datatype), value :: datatype
        type(MPI_Comm), value :: comm
        integer(C_INT), intent(out), optional :: ierror
    end subroutine MPI_Ssend

end module mpi

! A module of a host library, compiled without a GPU device (the
! integration_tests CMakeLists.txt drops `--gpu` for it), like the Fortran
! interface of HDF5 that a GPU program links against.
module gpu_deferred_sync_08_mod
implicit none
private
public :: extent_create, extent_dims, total
contains
subroutine extent_create(rank, dims, id)
    integer, intent(in) :: rank
    integer, intent(in) :: dims(*)
    integer, intent(out) :: id
    integer :: i
    id = 0
    do i = 1, rank
        id = 10*id + dims(i)
    end do
end subroutine

subroutine extent_dims(id, dims, maxdims)
    integer, intent(in) :: id
    integer, intent(out) :: dims(*)
    integer, intent(out) :: maxdims(*)
    dims(1) = id/10
    dims(2) = mod(id, 10)
    maxdims(1:2) = -1
end subroutine

subroutine total(x, s)
    real, intent(in) :: x(:)
    real, intent(out) :: s
    s = sum(x)
end subroutine
end module

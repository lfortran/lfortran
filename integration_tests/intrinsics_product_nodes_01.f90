program intrinsics_product_nodes
    use, intrinsic :: iso_fortran_env, only: int32
    implicit none

    integer, parameter :: ndim = 1
    integer(int32), dimension(ndim), parameter :: nodes = [10_int32]
    integer(int32), parameter :: ncol = product(nodes)

    if (ncol /= 10_int32) error stop
end program

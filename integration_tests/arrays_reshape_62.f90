module arrays_reshape_62_m
    implicit none
    real(8), allocatable :: eps(:), grid(:, :)
contains
    subroutine flatten(nx, ny, nz, wrk3d)
        integer, intent(in) :: nx, ny, nz
        real(8), intent(in) :: wrk3d(nx, ny, nz)
        eps = reshape(wrk3d, [nx*ny*nz])
    end subroutine flatten

    subroutine regrid(nx, ny, nz, wrk3d)
        integer, intent(in) :: nx, ny, nz
        real(8), intent(in) :: wrk3d(nx, ny, nz)
        grid = reshape(wrk3d, [nx*ny, nz])
    end subroutine regrid

    real(8) function total(nx, ny, nz, wrk3d)
        integer, intent(in) :: nx, ny, nz
        real(8), intent(in) :: wrk3d(nx, ny, nz)
        total = sum(reshape(wrk3d, [nx*ny*nz]))
    end function total
end module arrays_reshape_62_m

program arrays_reshape_62
    use arrays_reshape_62_m
    implicit none
    ! 16 MB of real(8): the reshaped copy must not live on the stack
    integer, parameter :: nx = 100, ny = 200, nz = 100
    real(8), allocatable :: w(:, :, :)
    integer :: i, j, k

    allocate (w(nx, ny, nz), eps(nx*ny*nz), grid(nx*ny, nz))
    do k = 1, nz
        do j = 1, ny
            do i = 1, nx
                w(i, j, k) = real(i + nx*(j - 1) + nx*ny*(k - 1), 8)
            end do
        end do
    end do

    call flatten(nx, ny, nz, w)
    if (size(eps) /= nx*ny*nz) error stop
    do i = 1, nx*ny*nz
        if (eps(i) /= real(i, 8)) error stop
    end do

    call regrid(nx, ny, nz, w)
    if (any(shape(grid) /= [nx*ny, nz])) error stop
    if (grid(1, 1) /= 1.0d0) error stop
    if (grid(nx*ny, 1) /= real(nx*ny, 8)) error stop
    if (grid(1, 2) /= real(nx*ny + 1, 8)) error stop
    if (grid(nx*ny, nz) /= real(nx*ny*nz, 8)) error stop

    w = 1.0d0
    if (total(nx, ny, nz, w) /= real(nx*ny*nz, 8)) error stop
    print *, 'ok', size(eps)
end program arrays_reshape_62

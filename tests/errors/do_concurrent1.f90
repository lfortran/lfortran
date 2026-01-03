program do_concurrent1
implicit none
integer, parameter :: Nx = 600, Ny = 450
integer :: i, j, image(Nx, Ny)
do concurrent (j = 1:Ny) local(i, j)
    do i = 1, Nx
    end do
end do
end program

module global_dims
    implicit none
    integer, parameter :: nt_g=5
    integer, parameter :: np_g=7
end module

program functions_35
    use global_dims
    implicit none
    real, dimension(:, :), allocatable, target :: br0_g
    allocate(br0_g(nt_g, np_g))
    call sub1(br0_g)
end program functions_35

subroutine sub1 (br0_g)
    use global_dims
    implicit none
    real, dimension(nt_g,np_g) :: br0_g
    real :: sum0
    call sub2 (nt_g-2,np_g-2, br0_g(2:4,2:6))
    sum0=sum(br0_g(2,2:6))
    print *, "sum0: ", sum0
    if (sum0 /= 1.21607137) error stop
end subroutine

subroutine sub2 (nx,ny,f)
    implicit none
    integer :: nx,ny
    real, dimension(nx,ny) :: f
    f = 2.43214264512062073e-01
    return
end subroutine

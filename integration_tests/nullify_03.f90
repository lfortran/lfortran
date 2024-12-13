module module_nullify_03
    implicit none
    integer, parameter, private :: mxdim = 3
    type :: rp1d
        real(8), dimension(:), pointer :: f
    end type

    type :: sds
        integer :: ndim
        integer, dimension(mxdim) :: dims
        logical :: hdf32
        type(rp1d), dimension(mxdim) :: scales
        real(8), dimension(:,:,:), pointer :: f
    end type
end module

program nullify_03
    use module_nullify_03
    implicit none
    type(sds) :: s
    ! allocate(s%scales(1)%f(2))
    ! s%scales(1)%f=[1,2]
    ! print *, 'Scale 0:', s%scales(1)%f
    nullify(s%scales(1)%f)
end program
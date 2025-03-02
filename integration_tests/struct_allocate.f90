module p
    implicit none
    integer, parameter, private :: mxdim = 3
    type :: rp1d
        real(8), dimension(:), pointer :: f
    end type

    type :: sds
        integer, dimension(:), pointer :: dims
        type(rp1d), dimension(mxdim) :: scales
        integer :: f
    end type
end module

program struct_allocate
    use p
    implicit none
    type(sds) :: s
    ! allocate(s%dims(2))
    ! allocate(s%scales(2)%f(2))
    ! s%scales(2)%f=[3,4]
    ! print *, 'Scale 0:', s%scales(2)%f(1)
    allocate(s%scales(1)%f(2))
    ! s%scales(1)%f=[1,2]
    ! print *, 'Scale 0:', s%scales(1)%f(1)
end program
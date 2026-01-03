module mod_struct_allocate
    implicit none
    integer, parameter, private :: mxdim = 3
    type :: rp1d
        real(8), dimension(:), pointer :: f
    end type

    type :: sds
        type(rp1d), dimension(mxdim) :: scales
    end type
end module

program struct_allocate
    use mod_struct_allocate
    implicit none
    type(sds) :: s
    allocate(s%scales(1)%f(4))
    allocate(s%scales(2)%f(3))
end program

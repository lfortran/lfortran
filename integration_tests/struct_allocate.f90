module mod_struct_allocate
    implicit none
    integer, parameter, private :: mxdim = 3
    type :: rp1d
        real(8), dimension(:), pointer :: f
    end type

    type :: sds
        type(rp1d), dimension(mxdim) :: scales
        integer, dimension(:), pointer ::dims
    end type
end module

program struct_allocate
    use mod_struct_allocate
    implicit none
    type(sds) :: s

    allocate(s%dims(2))
    s%dims=[10,11]
    print *, s%dims(1)

    allocate(s%scales(2)%f(3))
    s%scales(2)%f=[6,7,8]
    print *, s%scales(2)%f(3)
    
    allocate(s%scales(1)%f(2))
    s%scales(1)%f=[1,2]
    print *, s%scales(1)%f(1)
end program
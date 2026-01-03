module mm
    type t 
        integer,allocatable :: arr(:)
    end type
    type(t) :: instance_arr(39)
end module

program allocated_04
    use mm
    print *, allocated(instance_arr(1)%arr)
    if (allocated(instance_arr(1)%arr)) error stop
    allocate(instance_arr(1)%arr(10))
    print*, allocated(instance_arr(1)%arr)
    if (.not. allocated(instance_arr(1)%arr)) error stop
end program allocated_04
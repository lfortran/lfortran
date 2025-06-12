program allocatble_c_ptr
    use iso_c_binding, only: c_ptr
    implicit none
    type(c_ptr), allocatable :: c_requests(:)
    allocate(c_requests(4))
    deallocate(c_requests)
end program allocatble_c_ptr

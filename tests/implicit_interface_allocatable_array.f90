subroutine implicit_interface_allocatable_array
    implicit none
    real(4), dimension(:), allocatable :: r_g
    call sub(1, r_g)
end subroutine

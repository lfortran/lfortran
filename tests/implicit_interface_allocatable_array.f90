subroutine implicit_interface_allocatable_array
    implicit none
    real(4), dimension(:), allocatable :: r_g
    real(4), dimension(:), pointer :: r_t
    call sub1(1, r_g)
    call sub2(1, r_t)
end subroutine

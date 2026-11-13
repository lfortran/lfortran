module c_associated_fixed_component_01_m
    implicit none
    type :: box
        integer :: data(3)
        integer :: matrix(2, 3)
    end type
end module

program c_associated_fixed_component_01
    use c_associated_fixed_component_01_m
    implicit none
    type(box), target :: obj
    integer, target :: values(3), grid(2, 3)
    integer, pointer :: p(:), q(:, :)

    values(1) = 11
    grid(1, 1) = 17
    p => values(:)
    if (associated(p, obj%data)) error stop 1
    if (.not. associated(p, values)) error stop 2
    q => grid(:, :)
    if (associated(q, obj%matrix)) error stop 3
    if (.not. associated(q, grid)) error stop 4
    if (p(1) /= 11 .or. q(1, 1) /= 17) error stop 5

    p => null()
    if (associated(p) .or. associated(p, obj%data)) error stop 6
    q => null()
    if (associated(q, obj%matrix)) error stop 7
end program

module pointer_remap_component_01_m
    implicit none
    type :: holder_t
        real(8), pointer, contiguous :: values(:, :) => null()
        real(8), pointer, contiguous :: host(:) => null()
    end type holder_t
contains
    subroutine attach(self, m, q, offset)
        type(holder_t), intent(inout) :: self
        integer, intent(in) :: m, q, offset
        self%values(1:m, 1:q) => self%host(offset + 1:offset + m*q)
    end subroutine attach
end module pointer_remap_component_01_m

program pointer_remap_component_01
    ! Bounds-remapping pointer assignment to a component of a dummy and of a
    ! local derived-type variable.
    use pointer_remap_component_01_m
    implicit none
    type(holder_t) :: h
    integer :: i
    allocate(h%host(20))
    h%host = [(real(i, 8), i = 1, 20)]
    call attach(h, 3, 4, 5)
    if (any(shape(h%values) /= [3, 4])) error stop 1
    if (h%values(1, 1) /= 6.0d0 .or. h%values(3, 4) /= 17.0d0) error stop 2
    h%values(2, 3) = -1.0d0
    if (h%host(13) /= -1.0d0) error stop 3
    call attach(h, 2, 2, 0)
    if (any(shape(h%values) /= [2, 2]) .or. h%values(2, 2) /= 4.0d0) error stop 4
    h%values(0:1, 1:2) => h%host(3:6)
    if (any(lbound(h%values) /= [0, 1]) .or. h%values(1, 2) /= 6.0d0) error stop 5
    print *, sum(h%values)
end program pointer_remap_component_01

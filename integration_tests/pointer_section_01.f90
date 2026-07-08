program pointer_section_01
    implicit none

    integer, pointer :: t(:), u(:), v(:,:)
    integer, target :: t_target(5)
    integer :: i, j

    allocate(t(5))
    do i = 1, 5
        t(i) = i * 10
    end do

    ! Test 1: 1D section
    u(0:) => t(2:4)
    if (lbound(u, 1) /= 0) error stop 1
    if (ubound(u, 1) /= 2) error stop 2
    if (size(u) /= 3) error stop 3
    if (u(0) /= 20) error stop 4
    if (u(1) /= 30) error stop 5
    if (u(2) /= 40) error stop 6

    ! Test 2: Modify through pointer
    u(1) = 99
    if (t(3) /= 99) error stop 7

    ! Test 3: Re-associate with strides
    u(1:) => t(1:5:2)
    if (lbound(u, 1) /= 1) error stop 8
    if (ubound(u, 1) /= 3) error stop 9
    if (size(u) /= 3) error stop 10
    if (u(1) /= 10) error stop 11
    if (u(2) /= 99) error stop 12
    if (u(3) /= 50) error stop 13

    ! Clean up
    deallocate(t)

end program pointer_section_01

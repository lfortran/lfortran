module array_section_assign_15_m
    ! `x(:) = value` assigns to the whole array `x`, as `x = value` does
    ! without reallocation: no temporary for the value, no pointer to the
    ! section, the result of a function computed into `x` itself.
    implicit none
    type :: particle
        real, allocatable :: r(:), d(:)
    end type
    type :: tensor_t
        real, allocatable :: values_(:)
    end type
    type :: pair_t
        type(tensor_t) :: inputs_
    contains
        procedure :: inputs
    end type
contains
    elemental function inputs(self) result(t)
        class(pair_t), intent(in) :: self
        type(tensor_t) :: t
        t = self%inputs_
    end function

    pure function twice(a) result(r)
        real, intent(in) :: a(:)
        real :: r(size(a))
        r = 2*a
    end function

    subroutine self_update(h)
        real, allocatable, intent(inout) :: h(:)
        h(:) = 0.5*h + 0.25
    end subroutine

    subroutine pair_differences(p, i, j)
        type(particle), intent(inout) :: p(:)
        integer, intent(in) :: i, j
        p(i)%d(:) = p(j)%r - p(i)%r
    end subroutine

    subroutine elemental_results(pairs, in)
        type(pair_t), intent(in) :: pairs(:)
        type(tensor_t), allocatable, intent(inout) :: in(:)
        in(:) = pairs%inputs()
    end subroutine

    subroutine values(a, m, a2, b2, c1, c2, c3, c4)
        real, allocatable, intent(in) :: a(:), a2(:,:), b2(:,:)
        logical, intent(in) :: m(:)
        real, allocatable, intent(inout) :: c1(:), c2(:), c3(:), c4(:,:)
        c1(:) = twice(a(1:3))
        c2(:) = cshift(a(1:3), 1)
        c3(:) = pack(a, m)
        c4(:,:) = matmul(a2, b2)
    end subroutine
end module

program array_section_assign_15
    use array_section_assign_15_m
    implicit none
    real, parameter :: h_ref(4) = [0.75, 1.25, 1.75, 2.25]
    real, parameter :: c1_ref(3) = [2., 4., 6.], c2_ref(3) = [2., 3., 1.], &
        c3_ref(3) = [1., 3., 5.]
    real, allocatable :: h(:), a(:), a2(:,:), b2(:,:), c1(:), c2(:), c3(:), c4(:,:)
    logical, allocatable :: m(:)
    type(particle) :: p(2)
    type(pair_t) :: pairs(3)
    type(tensor_t), allocatable :: in(:)
    integer :: i, j

    allocate(h(4))
    do i = 1, 4
        h(i) = i
    end do
    call self_update(h)
    do i = 1, 4
        if (h(i) /= h_ref(i)) error stop 1
    end do

    do i = 1, 2
        allocate(p(i)%r(3), p(i)%d(3))
        do j = 1, 3
            p(i)%r(j) = i*j
            p(i)%d(j) = 0
        end do
    end do
    call pair_differences(p, 1, 2)
    call pair_differences(p, 2, 2)
    do j = 1, 3
        if (p(1)%d(j) /= j) error stop 2
        if (p(2)%d(j) /= 0) error stop 3
    end do

    allocate(in(3))
    do i = 1, 3
        allocate(pairs(i)%inputs_%values_(2), in(i)%values_(2))
        pairs(i)%inputs_%values_(1) = i
        pairs(i)%inputs_%values_(2) = 2*i
        in(i)%values_(1) = 0
        in(i)%values_(2) = 0
    end do
    call elemental_results(pairs, in)
    pairs(1)%inputs_%values_(1) = 7
    do i = 1, 3
        if (size(in(i)%values_) /= 2) error stop 4
        if (in(i)%values_(1) /= i .or. in(i)%values_(2) /= 2*i) error stop 5
    end do

    allocate(a(5), m(5), c1(3), c2(3), c3(3), a2(3,3), b2(3,3), c4(3,3))
    do i = 1, 5
        a(i) = i
        m(i) = mod(i, 2) == 1
    end do
    do j = 1, 3
        do i = 1, 3
            a2(i, j) = i
            b2(i, j) = j
        end do
    end do
    call values(a, m, a2, b2, c1, c2, c3, c4)
    do i = 1, 3
        if (c1(i) /= c1_ref(i)) error stop 6
        if (c2(i) /= c2_ref(i)) error stop 7
        if (c3(i) /= c3_ref(i)) error stop 8
        do j = 1, 3
            if (c4(i, j) /= 3*i*j) error stop 9
        end do
    end do
    print *, h, c3
end program

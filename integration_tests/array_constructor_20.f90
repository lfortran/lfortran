program array_constructor_20
    ! A component of a derived-type array in an array constructor, [a%x],
    ! when the lower bound of the array is not 1. The constructor holds the
    ! components of the elements of `a` in order, whatever its bounds are.
    implicit none
    type :: inner
        integer :: k
    end type
    type :: cell
        integer :: val
        character(len=2) :: tag
        type(inner) :: nest
    end type
    type(cell), allocatable :: a0(:), a5(:), an(:), a2(:, :), az(:)
    type(cell), pointer :: p0(:), p5(:), p2(:, :), view(:), remap(:)
    type(cell), target :: store(0:2)
    type(cell) :: fixed(0:2)
    integer :: got(3), flat(4), mixed(8), twice(8)
    character(len=2) :: tags(3)
    integer :: i, j

    ! Allocatable array, lower bound 0
    allocate(a0(0:2))
    do i = 0, 2
        a0(i)%val = 7 + i
        a0(i)%tag = achar(97 + i) // achar(65 + i)
        a0(i)%nest%k = 70 + 10*i
    end do
    got = [a0%val]
    if (any(got /= [7, 8, 9])) error stop 1

    ! Character component
    tags = [a0%tag]
    if (any(tags /= ["aA", "bB", "cC"])) error stop 2

    ! Component of a component
    got = [a0%nest%k]
    if (any(got /= [70, 80, 90])) error stop 3

    ! Mixed with scalars and another array, and used inside an expression
    mixed = [0, a0%val, 100, a0%nest%k]
    if (any(mixed /= [0, 7, 8, 9, 100, 70, 80, 90])) error stop 4
    if (sum([a0%val] * 2) /= 48) error stop 5
    if (size([a0%val]) /= 3) error stop 6

    ! Allocatable array, lower bound 5 and a negative lower bound
    allocate(a5(5:7))
    do i = 5, 7
        a5(i)%val = 10*i
        a5(i)%tag = "x" // achar(48 + i)
    end do
    got = [a5%val]
    if (any(got /= [50, 60, 70])) error stop 7
    tags = [a5%tag]
    if (any(tags /= ["x5", "x6", "x7"])) error stop 8

    allocate(an(-3:-1))
    do i = -3, -1
        an(i)%val = -i
    end do
    got = [an%val]
    if (any(got /= [3, 2, 1])) error stop 9

    ! Pointer array, lower bounds 0 and 5
    allocate(p0(0:2))
    do i = 0, 2
        p0(i)%val = 17 + i
        p0(i)%tag = achar(100 + i) // achar(68 + i)
        p0(i)%nest%k = 170 + 10*i
    end do
    got = [p0%val]
    if (any(got /= [17, 18, 19])) error stop 10
    tags = [p0%tag]
    if (any(tags /= ["dD", "eE", "fF"])) error stop 11
    got = [p0%nest%k]
    if (any(got /= [170, 180, 190])) error stop 12

    allocate(p5(5:7))
    do i = 5, 7
        p5(i)%val = 100*i
    end do
    got = [p5%val]
    if (any(got /= [500, 600, 700])) error stop 13

    ! Pointer associated with a target whose lower bound is 0
    do i = 0, 2
        store(i)%val = 27 + i
        store(i)%tag = achar(103 + i) // achar(71 + i)
    end do
    view => store
    if (lbound(view, 1) /= 0) error stop 14
    got = [view%val]
    if (any(got /= [27, 28, 29])) error stop 15
    tags = [view%tag]
    if (any(tags /= ["gG", "hH", "iI"])) error stop 16

    ! Pointer with remapped bounds
    remap(5:) => store
    if (lbound(remap, 1) /= 5) error stop 17
    got = [remap%val]
    if (any(got /= [27, 28, 29])) error stop 18

    ! Rank 2, allocatable and pointer
    allocate(a2(0:1, -1:0))
    allocate(p2(0:1, -1:0))
    do j = -1, 0
        do i = 0, 1
            a2(i, j)%val = 1 + i + 2*(j + 1)
            p2(i, j)%val = 11 + i + 2*(j + 1)
            p2(i, j)%tag = achar(49 + i) // achar(50 + j)
        end do
    end do
    flat = [a2%val]
    if (any(flat /= [1, 2, 3, 4])) error stop 19
    flat = [p2%val]
    if (any(flat /= [11, 12, 13, 14])) error stop 20
    if (any([p2%tag] /= ["11", "21", "12", "22"])) error stop 21

    ! Rank 2 component as the value of an implied do
    twice = [(a2%val, i = 1, 2)]
    if (any(twice /= [1, 2, 3, 4, 1, 2, 3, 4])) error stop 31
    twice = [(p2%val, i = 1, 2)]
    if (any(twice /= [11, 12, 13, 14, 11, 12, 13, 14])) error stop 32

    ! Zero-size array with a lower bound of 0
    allocate(az(0:-1))
    if (size([az%val]) /= 0) error stop 22

    ! Dummy arguments with a lower bound of 0
    call check_assumed_shape(a5)
    call check_allocatable(a0)
    call check_pointer(p0)

    ! Fixed-size array, lower bound 0
    do i = 0, 2
        fixed(i)%val = 37 + i
        fixed(i)%tag = achar(106 + i) // achar(74 + i)
    end do
    got = [fixed%val]
    if (any(got /= [37, 38, 39])) error stop 23
    tags = [fixed%tag]
    if (any(tags /= ["jJ", "kK", "lL"])) error stop 24

    deallocate(p0, p5, p2)
    print *, "OK"

contains

    subroutine check_assumed_shape(x)
        type(cell), intent(in) :: x(0:)
        integer :: r(3)
        if (lbound(x, 1) /= 0) error stop 25
        r = [x%val]
        if (any(r /= [50, 60, 70])) error stop 26
    end subroutine

    subroutine check_allocatable(x)
        type(cell), allocatable, intent(in) :: x(:)
        integer :: r(3)
        if (lbound(x, 1) /= 0) error stop 27
        r = [x%val]
        if (any(r /= [7, 8, 9])) error stop 28
    end subroutine

    subroutine check_pointer(x)
        type(cell), pointer, intent(in) :: x(:)
        integer :: r(3)
        if (lbound(x, 1) /= 0) error stop 29
        r = [x%val]
        if (any(r /= [17, 18, 19])) error stop 30
    end subroutine

end program

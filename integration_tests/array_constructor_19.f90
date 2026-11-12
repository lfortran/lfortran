program array_constructor_19
    implicit none
    type :: inner
        integer :: k
    end type
    type :: cell
        integer :: val
        character(len=2) :: name
        real :: w
        type(inner) :: nest
    end type
    type(cell), pointer :: row(:)
    type(cell), pointer :: view(:)
    type(cell), pointer :: grid(:, :)
    type(cell), target :: store(5)
    integer :: got(3), other(2), mixed(7), flat(4)
    character(len=2) :: names(3)
    real :: weights(3)
    integer :: i

    allocate(row(3))
    row%val = [7, 8, 9]
    row%name = ["ab", "cd", "ef"]
    row%w = [1.5, 2.5, 3.5]
    row%nest%k = [70, 80, 90]

    ! Integer component of a pointer array
    got = [row%val]
    if (any(got /= [7, 8, 9])) error stop 1

    ! Character component
    names = [row%name]
    if (any(names /= ["ab", "cd", "ef"])) error stop 2

    ! Real component
    weights = [row%w]
    if (any(abs(weights - [1.5, 2.5, 3.5]) > 1e-6)) error stop 3

    ! Component of a component
    got = [row%nest%k]
    if (any(got /= [70, 80, 90])) error stop 4

    ! Together with other arrays and scalars
    other = [1, 2]
    mixed = [other, row%val, 10, row(2)%val]
    if (any(mixed /= [1, 2, 7, 8, 9, 10, 8])) error stop 5

    ! The constructor is not the whole right-hand side
    got = [row%val] + 1
    if (any(got /= [8, 9, 10])) error stop 6
    if (sum([row%val]) /= 24) error stop 7
    if (size([row%val, row%val]) /= 6) error stop 8

    ! A pointer to a section with a stride
    do i = 1, 5
        store(i)%val = 10*i
        store(i)%name = "zz"
        store(i)%w = 0.0
        store(i)%nest%k = -i
    end do
    view => store(1:5:2)
    got = [view%val]
    if (any(got /= [10, 30, 50])) error stop 9
    got = [view%nest%k]
    if (any(got /= [-1, -3, -5])) error stop 10

    ! A pointer array of rank 2
    allocate(grid(2, 2))
    grid(1, 1)%val = 1
    grid(2, 1)%val = 2
    grid(1, 2)%val = 3
    grid(2, 2)%val = 4
    flat = [grid%val]
    if (any(flat /= [1, 2, 3, 4])) error stop 11

    ! A pointer dummy argument
    call collect(view, got)
    if (any(got /= [10, 30, 50])) error stop 12

    deallocate(row)
    deallocate(grid)
    print *, "OK"

contains

    subroutine collect(p, vals)
        type(cell), pointer, intent(in) :: p(:)
        integer, intent(out) :: vals(:)
        vals = [p%val]
    end subroutine
end program

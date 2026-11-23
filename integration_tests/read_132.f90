program read_132
    ! Formatted READ of a nested implied-do whose outer bound is a different
    ! component of the same object as the item (#18546). The values are
    ! stored column-major: 1, 3, 2, 4.
    implicit none
    type :: t
        integer :: n = 2
        integer, allocatable :: iz(:,:)
    end type
    type(t) :: b
    integer :: i, k, u
    allocate(b%iz(b%n, 2))
    open(newunit=u, status='scratch')
    write(u, '(4i3)') 1, 2, 3, 4
    rewind(u)
    read(u, '(4i3)') ((b%iz(i, k), k=1, 2), i=1, b%n)
    close(u)
    if (any(b%iz /= reshape([1, 3, 2, 4], [2, 2]))) error stop 1
end program

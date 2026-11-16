! An unallocated allocatable array as the data source of an allocatable
! component leaves the component unallocated (F2018 7.5.10), also when the
! data source has another type than the component, so that an allocated data
! source is converted.
program structure_constructor_args_36
implicit none
type :: holder
    real, allocatable :: d(:)
end type
type :: holder8
    real(8), allocatable :: d(:)
end type
type :: grid
    integer, allocatable :: m(:, :)
end type
type(holder) :: h
type(holder8) :: h8
type(grid) :: g
integer, allocatable :: ia(:)
real, allocatable :: ra(:), rm(:, :)
integer :: i

! The component is not allocated
h = holder(ia)
if (allocated(h%d)) error stop 1
h8 = holder8(ra)
if (allocated(h8%d)) error stop 2
g = grid(rm)
if (allocated(g%m)) error stop 3

! An allocated component is released
allocate(h%d(3), h8%d(0:2), g%m(2, 2))
h%d = 1.0
h8%d = 1.0d0
g%m = 1
h = holder(ia)
if (allocated(h%d)) error stop 4
h8 = holder8(ra)
if (allocated(h8%d)) error stop 5
g = grid(rm)
if (allocated(g%m)) error stop 6

! The data source is allocated in every other iteration
do i = 1, 4
    if (mod(i, 2) == 0) then
        allocate(ia(5:7), ra(0:2), rm(0:1, 3:4))
        ia = [4, 5, 6]
        ra = [1.0, 2.0, 3.0]
        rm = reshape([1.0, 2.0, 3.0, 4.0], [2, 2])
    end if
    h = holder(ia)
    h8 = holder8(ra)
    g = grid(rm)
    if (mod(i, 2) == 0) then
        if (.not. allocated(h%d)) error stop 10
        if (lbound(h%d, 1) /= 5 .or. ubound(h%d, 1) /= 7) error stop 11
        if (any(h%d /= [4.0, 5.0, 6.0])) error stop 12
        if (.not. allocated(h8%d)) error stop 13
        if (lbound(h8%d, 1) /= 0 .or. ubound(h8%d, 1) /= 2) error stop 14
        if (any(h8%d /= [1.0d0, 2.0d0, 3.0d0])) error stop 15
        if (.not. allocated(g%m)) error stop 16
        if (any(lbound(g%m) /= [0, 3]) .or. any(ubound(g%m) /= [1, 4])) error stop 17
        if (any(g%m /= reshape([1, 2, 3, 4], [2, 2]))) error stop 18
        deallocate(ia, ra, rm)
    else
        if (allocated(h%d)) error stop 20
        if (allocated(h8%d)) error stop 21
        if (allocated(g%m)) error stop 22
    end if
end do

print *, "ok"
end program

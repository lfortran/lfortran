! The bounds an allocatable array component takes from the data source of a
! structure constructor (F2018 7.5.10): lower bounds of 1 when the data source
! is an expression, whatever the bounds of its operands, and the bounds of a
! whole array also when its value is converted to the type of the component.
! Each case runs with the component not allocated, allocated with the bounds
! 5:7, 1:3 and 0:2, and allocated with another shape.
module structure_constructor_args_35_mod
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
type :: source_t
    real :: c(5:7)
    integer :: ic(5:7)
    real, allocatable :: a(:)
end type
type :: item
    real :: k = 0.0
end type
contains

subroutine prepare(h, state)
    type(holder), intent(inout) :: h
    integer, intent(in) :: state
    if (allocated(h%d)) deallocate(h%d)
    select case (state)
    case (1)
        allocate(h%d(5:7))
    case (2)
        allocate(h%d(1:3))
    case (3)
        allocate(h%d(0:2))
    case (4)
        allocate(h%d(2:6))
    end select
    if (allocated(h%d)) h%d = -1.0
end subroutine

subroutine prepare8(h, state)
    type(holder8), intent(inout) :: h
    integer, intent(in) :: state
    if (allocated(h%d)) deallocate(h%d)
    select case (state)
    case (1)
        allocate(h%d(5:7))
    case (2)
        allocate(h%d(1:3))
    case (3)
        allocate(h%d(0:2))
    case (4)
        allocate(h%d(2:6))
    end select
    if (allocated(h%d)) h%d = -1.0d0
end subroutine

subroutine prepare_grid(g, state)
    type(grid), intent(inout) :: g
    integer, intent(in) :: state
    if (allocated(g%m)) deallocate(g%m)
    select case (state)
    case (1)
        allocate(g%m(0:1, 3:4))
    case (2)
        allocate(g%m(1:2, 1:2))
    case (3)
        allocate(g%m(0:1, 1:2))
    case (4)
        allocate(g%m(3, 2))
    end select
    if (allocated(g%m)) g%m = -1
end subroutine

subroutine check(h, lb, expected, code)
    type(holder), intent(in) :: h
    integer, intent(in) :: lb, code
    real, intent(in) :: expected(:)
    if (.not. allocated(h%d)) error stop code
    if (lbound(h%d, 1) /= lb) error stop code
    if (ubound(h%d, 1) /= lb + size(expected) - 1) error stop code
    if (any(h%d /= expected)) error stop code
end subroutine

subroutine check8(h, lb, expected, code)
    type(holder8), intent(in) :: h
    integer, intent(in) :: lb, code
    real(8), intent(in) :: expected(:)
    if (.not. allocated(h%d)) error stop code
    if (lbound(h%d, 1) /= lb) error stop code
    if (ubound(h%d, 1) /= lb + size(expected) - 1) error stop code
    if (any(h%d /= expected)) error stop code
end subroutine

subroutine check_grid(g, lb, expected, code)
    type(grid), intent(in) :: g
    integer, intent(in) :: lb(2), code
    integer, intent(in) :: expected(:, :)
    if (.not. allocated(g%m)) error stop code
    if (any(lbound(g%m) /= lb)) error stop code
    if (ubound(g%m, 1) /= lb(1) + size(expected, 1) - 1) error stop code
    if (ubound(g%m, 2) /= lb(2) + size(expected, 2) - 1) error stop code
    if (any(g%m /= expected)) error stop code
end subroutine

elemental real function twice(x)
    real, intent(in) :: x
    twice = 2.0 * x
end function

! Expressions of dummy arguments with lower bounds other than 1
subroutine dummy_operands(assumed, explicit, n)
    integer, intent(in) :: n
    real, intent(in) :: assumed(5:), explicit(n:n + 2)
    type(holder) :: h
    integer :: state
    do state = 0, 4
        call prepare(h, state)
        h = holder(assumed + 1.0)
        call check(h, 1, [5.0, 6.0, 7.0], 300 + state)
        call prepare(h, state)
        h = holder(explicit + 1.0)
        call check(h, 1, [5.0, 6.0, 7.0], 310 + state)
        call prepare(h, state)
        h = holder(abs(explicit))
        call check(h, 1, [4.0, 5.0, 6.0], 320 + state)
    end do
end subroutine

end module

program structure_constructor_args_35
use structure_constructor_args_35_mod
implicit none
type(holder) :: h
type(holder8) :: h8
type(grid) :: g
type(source_t) :: t
type(item) :: items(5:7)
real :: vals(3), src(5:7), x(0:2), rm(0:1, 3:4)
real, allocatable :: alc(:)
real, parameter :: cst(3) = [4.0, 5.0, 6.0], shifted(5:7) = [4.0, 5.0, 6.0]
integer, parameter :: icst(2:4) = [7, 8, 9]
integer :: ivals(5:7), im(0:1, 3:4), state
integer, allocatable :: ialc(:)
logical :: mask(5:7)

vals = [1.0, 2.0, 3.0]
src = [4.0, 5.0, 6.0]
x = [1.0, 2.0, 3.0]
ivals = [4, 5, 6]
mask = [.true., .false., .true.]
t%c = src
t%ic = ivals
allocate(t%a(5:7))
t%a = src
allocate(alc(5:7))
alc = src
allocate(ialc(5:7))
ialc = ivals
items%k = src
im = reshape([1, 2, 3, 4], [2, 2])
rm = real(im)

! The reported cases: a component with lower bound 1 keeps it for an
! expression of an array with other bounds
h = holder(vals)
h = holder(src + 1.0)
call check(h, 1, [5.0, 6.0, 7.0], 1)
h = holder(vals)
h = holder(abs(src))
call check(h, 1, [4.0, 5.0, 6.0], 2)
h = holder(vals)
h = holder(real(ivals))
call check(h, 1, [4.0, 5.0, 6.0], 3)
! and a component with the bounds of a whole array that is converted keeps them
deallocate(h%d)
allocate(h%d(5:7))
h%d = 0.0
h = holder(ivals)
call check(h, 5, [4.0, 5.0, 6.0], 4)
allocate(h8%d(0:2))
h8%d = 0.0d0
h8 = holder8(x)
call check8(h8, 0, [1.0d0, 2.0d0, 3.0d0], 5)

do state = 0, 4
    ! An expression has lower bounds of 1
    call prepare(h, state)
    h = holder(src + 1.0)
    call check(h, 1, [5.0, 6.0, 7.0], 10 + state)
    call prepare(h, state)
    h = holder(-src)
    call check(h, 1, [-4.0, -5.0, -6.0], 20 + state)
    call prepare(h, state)
    h = holder(abs(src))
    call check(h, 1, [4.0, 5.0, 6.0], 30 + state)
    call prepare(h, state)
    h = holder(real(ivals))
    call check(h, 1, [4.0, 5.0, 6.0], 40 + state)
    call prepare(h, state)
    h = holder(merge(src, 0.0, mask))
    call check(h, 1, [4.0, 0.0, 6.0], 50 + state)
    call prepare(h, state)
    h = holder(twice(src))
    call check(h, 1, [8.0, 10.0, 12.0], 60 + state)
    call prepare(h, state)
    h = holder(t%c + 1.0)
    call check(h, 1, [5.0, 6.0, 7.0], 70 + state)
    call prepare(h, state)
    h = holder(t%a + 1.0)
    call check(h, 1, [5.0, 6.0, 7.0], 80 + state)
    call prepare(h, state)
    h = holder(src * alc)
    call check(h, 1, [16.0, 25.0, 36.0], 90 + state)
    call prepare(h, state)
    h = holder(alc + 1.0)
    call check(h, 1, [5.0, 6.0, 7.0], 100 + state)
    ! as has the component of an array of derived type, which is not a
    ! whole array
    call prepare(h, state)
    h = holder(items%k)
    call check(h, 1, [4.0, 5.0, 6.0], 110 + state)
    ! and an expression that is converted
    call prepare(h, state)
    h = holder(ivals + 1)
    call check(h, 1, [5.0, 6.0, 7.0], 120 + state)
    call prepare(h, state)
    h = holder(ivals(5:7))
    call check(h, 1, [4.0, 5.0, 6.0], 130 + state)
    call prepare8(h8, state)
    h8 = holder8(src + 1.0)
    call check8(h8, 1, [5.0d0, 6.0d0, 7.0d0], 140 + state)
    ! and a constant expression
    call prepare(h, state)
    h = holder(shifted + 1.0)
    call check(h, 1, [5.0, 6.0, 7.0], 150 + state)
    call prepare8(h8, state)
    h8 = holder8(cst + 1.0)
    call check8(h8, 1, [5.0d0, 6.0d0, 7.0d0], 160 + state)

    ! A whole array that is converted keeps its bounds
    call prepare(h, state)
    h = holder(ivals)
    call check(h, 5, [4.0, 5.0, 6.0], 200 + state)
    call prepare(h, state)
    h = holder(ialc)
    call check(h, 5, [4.0, 5.0, 6.0], 210 + state)
    call prepare(h, state)
    h = holder(t%ic)
    call check(h, 5, [4.0, 5.0, 6.0], 220 + state)
    call prepare8(h8, state)
    h8 = holder8(x)
    call check8(h8, 0, [1.0d0, 2.0d0, 3.0d0], 230 + state)
    call prepare8(h8, state)
    h8 = holder8(alc)
    call check8(h8, 5, [4.0d0, 5.0d0, 6.0d0], 240 + state)
    call prepare8(h8, state)
    h8 = holder8(ivals)
    call check8(h8, 5, [4.0d0, 5.0d0, 6.0d0], 250 + state)
    ! as does a named constant that is converted
    call prepare(h, state)
    h = holder(icst)
    call check(h, 2, [7.0, 8.0, 9.0], 290 + state)
    call prepare8(h8, state)
    h8 = holder8(cst)
    call check8(h8, 1, [4.0d0, 5.0d0, 6.0d0], 295 + state)

    ! Rank 2
    call prepare_grid(g, state)
    g = grid(rm)
    call check_grid(g, [0, 3], im, 260 + state)
    call prepare_grid(g, state)
    g = grid(im + 1)
    call check_grid(g, [1, 1], im + 1, 270 + state)
    call prepare_grid(g, state)
    g = grid(int(rm))
    call check_grid(g, [1, 1], im, 280 + state)
end do

call dummy_operands(src, src, 5)

print *, "ok"
end program

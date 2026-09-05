! An array of a derived type with an allocatable component, passed through
! a procedure pointer. LLVM lays the element type out from FunctionType
! with no expression, so an allocatable member must be a descriptor rather
! than an array element type.
program derived_types_159
implicit none
type :: t
    integer, allocatable :: a(:)
    integer :: n
end type
type(t) :: xs(2)
procedure(work), pointer :: p
integer :: i

do i = 1, 2
    allocate(xs(i)%a(2))
    xs(i)%a = [i, i + 10]
    xs(i)%n = i
end do

p => work
call p(xs)

if (xs(1)%a(1) /= 2) error stop 1
if (xs(1)%a(2) /= 12) error stop 2
if (xs(2)%n /= 12) error stop 3
if (.not. allocated(xs(2)%a)) error stop 4
if (xs(2)%a(1) /= 2) error stop 5
if (storage_size(xs(1)) /= sizeof(xs(1)) * 8) error stop 6

contains

subroutine work(arr)
    type(t), intent(inout) :: arr(:)
    arr(1)%a = arr(1)%a + 1
    arr(2)%n = arr(2)%n + 10
end subroutine

end program

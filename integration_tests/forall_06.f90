program forall_06
implicit none
character(3) :: x(3) = ['dog', 'cat', 'boy']
character(4) :: y(1) = ['cat ']
integer :: i, j, numbers(3) = [1, 2, 3], member(1) = [2]
logical :: mask(3), matrix(3, 2)
logical, allocatable :: empty(:)

forall(i=1:3) mask(i) = all(y /= x(i))
if (any(mask .neqv. [.true., .false., .true.])) error stop 1
forall(i=1:3) mask(i) = any(y == x(i))
if (any(mask .neqv. [.false., .true., .false.])) error stop 2
forall(i=1:3) mask(i) = all(member /= numbers(i))
if (any(mask .neqv. [.true., .false., .true.])) error stop 3

! Arguments to a pure call and both indices must remain inside the iteration.
forall(i=1:3, j=1:2) matrix(i,j) = contains(member + j - 1, numbers(i))
if (any(matrix(:,1) .neqv. [.false., .true., .false.])) error stop 4
if (any(matrix(:,2) .neqv. [.false., .false., .true.])) error stop 5

! The mask guards RHS evaluation, and an empty range never reads x(i).
mask = .false.
forall(i=1:3, i == 2) mask(i) = any(y == x(i))
if (any(mask .neqv. [.false., .true., .false.])) error stop 6
mask = .true.
forall(i=1:0) mask(i) = any(y == x(i))
if (.not. all(mask)) error stop 7

! Reductions over an empty array have their identity values per iteration.
allocate(empty(0))
forall(i=1:3) mask(i) = all(empty .eqv. (numbers(i) == 2))
if (.not. all(mask)) error stop 8
forall(i=1:3) mask(i) = any(empty .eqv. (numbers(i) == 2))
if (any(mask)) error stop 9

contains
pure logical function contains(values, value)
integer, intent(in) :: values(:), value
contains = any(values == value)
end function
end program

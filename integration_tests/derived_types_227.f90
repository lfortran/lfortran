program derived_types_227
! Intrinsic assignment of a derived type with a deferred-length
! character array component must copy the run-time element length.
implicit none
type :: v
    character(len=:), allocatable :: d(:)
end type
type :: w
    type(v) :: inner
    integer :: k
end type
type, extends(v) :: ext
    integer :: k = 0
end type
type(v) :: a, b, c, e
type(v) :: arr1(2), arr2(2)
type(w) :: wa, wb
type(ext) :: x
class(v), allocatable :: y

! unallocated component
b = a
if (allocated(b%d)) error stop 1

allocate(character(len=2) :: a%d(3))
a%d(1) = 'ab'; a%d(2) = 'cd'; a%d(3) = 'ef'
b = a
if (len(b%d) /= 2) error stop 2
if (size(b%d) /= 3) error stop 3
if (b%d(1) /= 'ab' .or. b%d(2) /= 'cd' .or. b%d(3) /= 'ef') error stop 4

! the copy is independent of the source
a%d(1) = 'zz'
if (b%d(1) /= 'ab') error stop 5

! source reallocated with a different length, destination already allocated
deallocate(a%d)
allocate(character(len=5) :: a%d(2))
a%d(1) = 'hello'; a%d(2) = 'world'
b = a
if (len(b%d) /= 5) error stop 6
if (size(b%d) /= 2) error stop 7
if (b%d(1) /= 'hello' .or. b%d(2) /= 'world') error stop 8

! zero-size component
allocate(character(len=4) :: c%d(0))
e = c
if (.not. allocated(e%d)) error stop 9
if (size(e%d) /= 0) error stop 10
if (len(e%d) /= 4) error stop 11

! unallocated source over an allocated destination
deallocate(c%d)
b = c
if (allocated(b%d)) error stop 12

! array of derived types
allocate(character(len=3) :: arr1(1)%d(2))
arr1(1)%d(1) = 'abc'; arr1(1)%d(2) = 'def'
allocate(character(len=1) :: arr1(2)%d(1))
arr1(2)%d(1) = 'q'
arr2 = arr1
if (len(arr2(1)%d) /= 3 .or. arr2(1)%d(2) /= 'def') error stop 13
if (len(arr2(2)%d) /= 1 .or. arr2(2)%d(1) /= 'q') error stop 14

! nested component
allocate(character(len=4) :: wa%inner%d(2))
wa%inner%d(1) = 'abcd'; wa%inner%d(2) = 'efgh'
wa%k = 7
wb = wa
if (len(wb%inner%d) /= 4 .or. wb%inner%d(2) /= 'efgh' .or. wb%k /= 7) error stop 15

! polymorphic copy through allocate(source=)
allocate(character(len=3) :: x%d(2))
x%d(1) = 'abc'; x%d(2) = 'xyz'
x%k = 4
allocate(y, source=x)
if (len(y%d) /= 3 .or. y%d(1) /= 'abc' .or. y%d(2) /= 'xyz') error stop 16
select type (y)
type is (ext)
    if (y%k /= 4) error stop 17
class default
    error stop 18
end select

print *, "ok"
end program

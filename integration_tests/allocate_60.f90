program allocate_60
implicit none

type :: MyType
    integer :: val = 0
end type MyType

type(MyType), allocatable :: arr(:)
integer :: call_count

call_count = 0
allocate(arr, source = allocator())

if (call_count /= 1) error stop
if (size(arr) /= 9) error stop
if (arr(1)%val /= 42) error stop
if (arr(2)%val /= 0) error stop
print *, "ok"

contains

function allocator() result(arr)
    type(MyType), allocatable :: arr(:)
    call_count = call_count + 1
    allocate(arr(9))
    arr(1) = MyType(42)
end function allocator

end program allocate_60

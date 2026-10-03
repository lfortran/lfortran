! Companion to implicit_interface_64, compiled as a separate unit so the caller
! has to synthesize an implicit interface from the actual argument alone.
subroutine take_assumed_size(arr)
    implicit none
    character(len=*) :: arr(*)
    if (len(arr(1)) /= 5) error stop 11
    if (arr(1) /= 'aa') error stop 12
    if (arr(2) /= 'bb') error stop 13
    if (arr(3) /= 'cc') error stop 14
end subroutine take_assumed_size

subroutine take_assumed_shape_len(arr, n)
    implicit none
    integer :: n
    character(len=*) :: arr(n)
    if (len(arr(1)) /= 5) error stop 21
    if (size(arr) /= 3) error stop 22
    if (arr(1) /= 'aa') error stop 23
    if (arr(3) /= 'cc') error stop 24
end subroutine take_assumed_shape_len

subroutine take_scalar(t)
    implicit none
    character(len=*) :: t
    if (len(t) /= 5) error stop 31
    if (t /= 'hello') error stop 32
end subroutine take_scalar

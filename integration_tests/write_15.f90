program write_15
    implicit none

    character(len=:), allocatable :: arr(:)
    character(len=100) :: buffer
    integer :: i

    arr = [character(len=5) :: "a", "b", "c"]

    buffer = ""
    write(buffer, "(*(A,1X))") (trim(arr(i)), i=1, size(arr))
    if (trim(buffer) /= "a b c") error stop "Expected: a b c"

    print *, "PASS: implied-do write of trim(arr(i)) works"
end program write_15

program write_15
    implicit none

    character(len=:), allocatable :: arr(:)
    character(len=100) :: buffer
    character(len=3) :: compact
    integer :: i, j, k

    arr = [character(len=5) :: "a", "b", "c"]

    buffer = ""
    write(buffer, "(*(A,1X))") (trim(arr(i)), i=1, size(arr))

    compact = ""
    k = 0
    do j = 1, len_trim(buffer)
        if (buffer(j:j) /= " ") then
            k = k + 1
            if (k <= len(compact)) compact(k:k) = buffer(j:j)
        end if
    end do
    if (compact /= "abc") error stop "Expected letters: abc"

    print *, "PASS: implied-do write of trim(arr(i)) works"
end program write_15

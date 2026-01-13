program implied_do_string_01
    ! MRE for LLVM dominance violation in string deallocation
    ! Tests implied-do with deferred-length strings
    implicit none
    character(len=:), allocatable :: arr(:)
    character(len=100) :: buffer
    integer :: i

    arr = [character(len=5) :: "a", "b", "c"]
    buffer = ""
    write(buffer, "(*(A,1X))") (trim(arr(i)), i=1, size(arr))
    if (trim(buffer) /= "a b c ") error stop "Expected: a b c "
    print *, "PASS"
end program

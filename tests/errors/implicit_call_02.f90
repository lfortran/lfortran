! Test: Array kind mismatch via implicit interface
! With --implicit-interface --implicit-argument-casting, passing integer(8)
! array to integer(4) parameter should produce semantic error.
program implicit_call_02
    implicit none
    integer(8) :: arr(2)
    arr(1) = 100
    arr(2) = 200
    call test_sub(arr)
end program

subroutine test_sub(x)
    implicit none
    integer(4) :: x(2)
    print *, x(1), x(2)
end subroutine

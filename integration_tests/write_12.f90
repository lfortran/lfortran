program write_12
    implicit none
    integer :: x
    x = 42
    write(6, '(A,I0)') "x = ", x
    print *, "PASS"
end program

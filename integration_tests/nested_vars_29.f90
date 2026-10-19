! A forall bound and an inner do concurrent bound that call an internal function
! which reads a variable of the host (#14903).
program nested_vars_29
    implicit none
    integer :: n, i, j, a(4), b(3,3)
    n = 3
    a = 0
    forall (i = 1:f(4)) a(i) = i
    if (any(a /= [1, 2, 3, 0])) error stop 1
    b = 0
    do concurrent (i = 1:3)
        do concurrent (j = 1:f(i))
            b(i,j) = 1
        end do
    end do
    if (sum(b) /= 6) error stop 2
    print *, a, b
contains
    pure integer function f(k)
        integer, intent(in) :: k
        f = min(k, n)
    end function
end program

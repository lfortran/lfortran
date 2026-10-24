! A FORALL construct with two statements nested in a FORALL (#15602)
program forall_14
    implicit none
    integer :: i, j, a(3,3), c(3,3)
    a = 0
    c = 0
    forall (i = 1:3)
        forall (j = 1:i)
            a(i,j) = i + 10*j
            c(j,i) = a(i,j)
        end forall
    end forall
    print *, a
    print *, c
    if (any(a /= reshape([11, 12, 13, 0, 22, 23, 0, 0, 33], [3, 3]))) error stop
    if (any(c /= transpose(a))) error stop
end program

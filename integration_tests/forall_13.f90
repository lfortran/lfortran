! A nested FORALL whose inner bound uses the outer index through an array
! expression (#15269). The inner bound is evaluated for each outer index.
! GFortran gives wrong results for this FORALL, so this test is not run with it.
program forall_13
implicit none
integer :: z(3,3), i, j, w(3)
z = 0
w = 1
forall (i = 1:3)
    forall (j = 1:sum(w(1:i))) z(i,j) = i + 10*j
end forall
if (any(z /= reshape([11, 12, 13, 0, 22, 23, 0, 0, 33], [3, 3]))) error stop
print *, z
end program

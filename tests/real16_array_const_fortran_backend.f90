program decl16
implicit none
real(16) :: v(2) = [1.5_16, 2.5_16]
real(16) :: q(2,2) = reshape([1.5_16, 2.5_16, 3.5_16, 4.5_16], [2,2])
real(16), parameter :: p(2) = [1.5_16, 2.5_16]
print *, v, q, p
end program

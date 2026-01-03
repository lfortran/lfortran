program arrays_80

real :: A(5, 10), g(5)

A = 1.0
g = 2.0

print *, trstlp(A, g)
if( abs(trstlp(A, g) - 60.0) > 1e-8 ) error stop
if( abs(trstlp(A, g) - sum(A) - sum(g)) > 1e-8 ) error stop

contains

function trstlp(A, g) result(d)
real :: A(:,:), g(:)
integer :: n, m
real :: A_aug(size(A, 1), size(A, 2) + 1)

real :: d

m = size(A, 2) + 1
n = size(A, 1)

A_aug = reshape([A, g], [n, m])
d = sum(A_aug)
end function

end program

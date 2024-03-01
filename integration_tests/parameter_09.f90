program parameter_09
real, parameter :: D(*) = [1.0, 2.0] + 2.0
real, parameter :: A(*) = sin(D) * 9.0
integer, parameter :: B(*) = [1, 2] + 2
real, parameter :: C(*) = sin(A) + cos(D) * 3.15 / 2.0
real, parameter :: E(*) = [1.0, 2.0] + [2.0, 3.0]
complex, parameter :: F(*) = [(1.0, 2.0), (3.0, 4.0), (124.1, 14.5)] + [(5.0, 6.0), (7.0, 8.0), (7.0, 8.0)]
complex , parameter :: G(*) = 3.0 / cos(C) + tan(A) * 6.0 / sin(D)
print *, sum(A)
if (abs(sum(A) - (-5.54114246)) > 1e-8) error stop
print *, sum(D)
if (abs(sum(D) - 7.0) > 1e-8) error stop
print *, sum(B)
if (sum(B) /= 7) error stop
print *, sum(C)
if (abs(sum(C) - (-2.13744116)) > 1e-8) error stop
print *, sum(E)
if (abs(sum(E) - 8.0) > 1e-8) error stop
print *, abs(sum(F))
if (abs(abs(sum(F)) - 153.116501) > 1e-8) error stop
print *, abs(sum(G))
if (abs(abs(sum(G)) - 225.452) > 1e-3) error stop
end program

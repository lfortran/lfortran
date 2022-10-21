program conv_complex2real
complex Z
real x
Z = (3.0, 4.0)
X=DBLE(Z)
if (abs(X-3.0) > 1e-5) error stop
end

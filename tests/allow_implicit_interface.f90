double precision function prho()
double precision zero, one, two, b, x, y, u, six
data zero, one, two, six /0.0d0, 1.0d0, 2.0d0, 6.0d0/

prho = u / exp(y / two) + alnorm(x, .true.)
external dgetrf
call dgetrf(x, y, u)
if (prho < zero) prho = zero
if (prho > one) prho = one
return
end

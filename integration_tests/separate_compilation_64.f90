program separate_compilation_64
use separate_compilation_64a, only: o, os, t, pf_t
implicit none
type(pf_t) :: v
v = o%inner
if (v%f(7) /= 14) error stop
v = os(1)%inner
if (v%f(7) /= 21) error stop
v = t%mid%inner
if (v%f(5) /= 15) error stop
print *, "ok"
end program

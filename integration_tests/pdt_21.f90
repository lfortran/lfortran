! Default initializers of components that depend on a kind type parameter
program pdt_21
implicit none

type :: w(k)
    integer, kind :: k = 3
    integer :: d = k + 1
    integer :: e = 2*k - 1
    integer :: f = -k
    integer :: g = max(k, 4)
    real :: r(k+1) = 1.5
    integer :: c(k, k) = 7
end type

type(w) :: s
type(w(5)) :: t

if (s%d /= 4 .or. s%e /= 5 .or. s%f /= -3 .or. s%g /= 4) error stop 1
if (t%d /= 6 .or. t%e /= 9 .or. t%f /= -5 .or. t%g /= 5) error stop 2
if (size(s%r) /= 4 .or. any(s%r /= 1.5)) error stop 3
if (size(t%r) /= 6 .or. any(t%r /= 1.5)) error stop 4
if (any(shape(s%c) /= [3, 3]) .or. sum(s%c) /= 63) error stop 5
if (any(shape(t%c) /= [5, 5]) .or. sum(t%c) /= 175) error stop 6
print *, s%d, s%e, s%f, s%g, t%d, t%e, t%f, t%g, size(s%r), size(t%r)
end program

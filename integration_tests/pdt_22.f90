! A kind type parameter used in the bounds of an array component of a
! parameterized derived type that is a component of another one
module pdt_22_mod
implicit none

type :: w(k)
    integer, kind :: k = 3
    integer :: c(k)
end type

type :: outer(n)
    integer, kind :: n = 2
    type(w(n)) :: inner
    type(w(n)), allocatable :: list(:)
end type

end module

program pdt_22
use pdt_22_mod, only: mouter => outer
implicit none

type :: w(k)
    integer, kind :: k = 3
    integer :: c(k)
end type

type :: outer(n)
    integer, kind :: n = 2
    type(w(n)) :: inner
    type(w(n)), allocatable :: list(:)
end type

type(outer) :: o
type(outer(5)) :: o5
type(mouter) :: mo
type(mouter(5)) :: mo5

if (size(o%inner%c) /= 2) error stop 1
if (size(o5%inner%c) /= 5) error stop 2
o5%inner%c = [1, 2, 3, 4, 5]
if (sum(o5%inner%c) /= 15) error stop 3
allocate(o%list(3), o5%list(2))
if (size(o%list(3)%c) /= 2) error stop 4
if (size(o5%list(1)%c) /= 5) error stop 5

if (size(mo%inner%c) /= 2) error stop 6
if (size(mo5%inner%c) /= 5) error stop 7
mo5%inner%c = 4
if (sum(mo5%inner%c) /= 20) error stop 12
allocate(mo5%list(4))
if (size(mo5%list(4)%c) /= 5) error stop 13
print *, size(o%inner%c), size(o5%inner%c)
end program

! Length type parameters with default values, keyword values, together with
! kind type parameters, and as character lengths of components

program pdt_26
implicit none

type :: named(k, l)
    integer, kind :: k = 4
    integer, len :: l = 3
    character(len=l) :: name
    real(k) :: v(l, 2)
end type

type :: grid(rows, cols)
    integer, len :: cols = 2, rows = 3
    real :: x(rows, cols)
end type


type(named) :: d
type(named(8, 5)) :: e
type(named(l=2)) :: f
type(grid) :: g
type(grid(4, 5)) :: h
type(grid(cols=1)) :: o

d%name = "abcdef"
e%name = "abcdef"
f%name = "abcdef"
e%v = 2.5_8
if (d%k /= 4 .or. d%l /= 3 .or. len(d%name) /= 3 .or. d%name /= "abc") error stop 1
if (e%k /= 8 .or. e%l /= 5 .or. len(e%name) /= 5 .or. e%name /= "abcde") error stop 2
if (f%k /= 4 .or. f%l /= 2 .or. len(f%name) /= 2 .or. f%name /= "ab") error stop 3
if (kind(e%v) /= 8 .or. any(shape(e%v) /= [5, 2]) .or. any(e%v /= 2.5_8)) error stop 4
if (kind(f%v) /= 4 .or. any(shape(f%v) /= [2, 2])) error stop 5

if (g%rows /= 3 .or. g%cols /= 2 .or. any(shape(g%x) /= [3, 2])) error stop 6
if (h%rows /= 4 .or. h%cols /= 5 .or. any(shape(h%x) /= [4, 5])) error stop 7
if (o%rows /= 3 .or. o%cols /= 1 .or. any(shape(o%x) /= [3, 1])) error stop 8
print *, d%name, " ", e%name, " ", f%name, shape(g%x), shape(h%x), shape(o%x)
end program

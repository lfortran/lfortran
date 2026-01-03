integer function a(d, e)
implicit integer (a,b-c)
implicit integer*4 (d-e)
implicit integer*8 (f-g)
implicit real (h)
implicit real*4 (i-k)
implicit real*8 (l)
implicit complex (m, n)
implicit complex*8 (o)
implicit complex*16 (p)
implicit double precision (q)
implicit double precision (r)
integer :: Y(e)
real :: X(d, e, e)
real :: b
Y(1) = 3
X(1, 1, 1) = 3
a = 1
b = 2
d = 3
f = 4
h = 5
i = 6
l = 7
m = 8
o = 9
p = 10
q = 11
r = 12
end function
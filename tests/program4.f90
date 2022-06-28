program program4
real :: x, y, z
x = 1.0
y = 2.0

z = func1(x, y)
print *, "6.0 == ", z

z = func1(x, y)
print *, "7.0 == ", z

z = func2(x, y)
print *, "6.0 == ", z

z = func2(x, y)
print *, "7.0 == ", z
contains

function func1(a, b) result(c)
    real, intent(in) :: a, b
    real :: c
    real, save :: saved = 2.0
    saved = saved + 1.0
    c = c + a + b + saved
end function func1

function func2(a, b) result(c)
    real, intent(in) :: a, b
    real :: c
    real :: saved = 2.0
    saved = saved + 1.0
    c = c + a + b + saved
end function func2
end program program4
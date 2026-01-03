program program3
real :: x, y, z

x = 5.0
y = 2.0
z = x*y
print *, "10.0 ==", z

z = func1(x, y)
print *, "49.0 ==", z

z = func2(x, y)
print *, "10.0 ==", z

contains

function func1(a, b) result(c)
    real, intent(in) :: a, b
    real :: c
    c = a + b
    c = func2(c, c)
end function

function func2(a, b) result(c)
    real, intent(in) :: a, b
    real :: c
    c = a * b
    ! c = func1(c, c)
end function

end program program3
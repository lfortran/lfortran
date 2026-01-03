program array_64
integer :: X(2)
X = [0,1]
X(X + 1) = 11235
print *, X
if ( any(X /= 11235) ) error stop
end program

program max2
double precision x, y, z, s
s = max(x, y, z)
if(abs(x-abs(y))>1e-12) error stop
end program
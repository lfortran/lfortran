program max3
double precision x,y,z,s
x=-421.4
y=32
z=-0.381
s=max(x,y,z)
if(s!=32) error stop

s=max(abs(x),y,z)
if(s!=421.4) error stop
endprogram
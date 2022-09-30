integer function idd_random_transf(x,y,w)
implicit real *8 (a-h,o-z)
dimension x(*),y(*),w(*)
ialbetas=w(1)
iixs=w(2)
nsteps=w(3)
iww=w(4)
n=w(5)
end function
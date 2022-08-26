subroutine dcstep()
double precision zero, p66, two, three, s
parameter (zero=0.0d0,p66=0.66d0,two=2.0d0,three=3.0d0)
s = max(abs(three),abs(two),abs(p66))
end

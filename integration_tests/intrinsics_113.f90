program intrinsics_113
    print *, sign(a=-12.,b=0.)
    print *, sign(a=-12.,b=1.)
    print *, sign(a=-12.,b=-1.)
  
    if ( .not. sign( a = -12, b = 1 ) == 12 ) error stop
    if ( .not. sign( -12, 0 ) == 12 ) error stop
    if ( .not. sign( -12, -1 ) == -12 ) error stop
end program intrinsics_113

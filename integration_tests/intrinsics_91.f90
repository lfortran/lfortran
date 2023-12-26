program intrinsics_91
    if ( index('abc', 'b') /= 2 ) error stop
    if ( index("elephant", "an") /= 6 ) error stop
    if ( index("potato", "t", back=.true.) /= 5 ) error stop
    if ( index("potato", "t", back=.false.) /= 3 ) error stop
    if ( index("potato", "ta", back=.true.) /= 3 ) error stop
end
    
    
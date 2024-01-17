program intrinsics_111
    integer :: a, b, result
    a = 10
    b = 7
    if ( .not. min(a1=a, a2=b) == 7) error stop
end program intrinsics_111

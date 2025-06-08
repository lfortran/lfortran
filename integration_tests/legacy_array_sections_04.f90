program legacy_array_sections_04
    real :: afb(4)
    external :: sub
    afb = [ 1., 2., 3., 4. ]
    call sub(afb( 2 ))
    call sub(afb)
end program

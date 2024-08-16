program equivalence_09
    real               ci( 2, 2 ), civ( 4 ), cr( 2, 2 ), crv( 4 )
    equivalence        ( ci( 1, 1 ), civ( 1 ) ), ( cr( 1, 1 ), crv( 1 ) )
    
    ci(1,1) = 5
    if(civ(1) /= 5) error stop

    cr(1,1) = 7
    if(crv(1) /= 7) error stop
end
     
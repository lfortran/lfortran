program bindc5
    real               afb(4)
    afb = [ 1., 2., 3., 4. ]
    call sub(afb     , 1, 1.)
    call sub(afb     , 3, 3.)

    call sub(afb( 1 ), 1, 1.)
    call sub(afb( 1 ), 3, 3.)

    call sub(afb( 2 ), 1, 2.)
    call sub(afb( 2 ), 3, 4.)

    call sub(afb( 3 ), 1, 3.)
    call sub(afb( 3 ), 2, 4.)

    call sub(afb( 4 ), 1, 4.)

    contains
        subroutine sub(afb, i, r) bind(c)
            real afb(*)
            integer i
            real r
            if ( afb(i) /= r ) error stop
        end subroutine
end program

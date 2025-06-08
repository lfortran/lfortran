        PROGRAM data_stmt_with_implied_do_loop
        IMPLICIT NONE
        INTEGER i
        INTEGER P(1), C(1,1) 
        CHARACTER(1) :: ch
        DATA P( 1), (C( 1,i), i=1, 1) /     31, 12 /
        DATA ch / "=" /
        END PROGRAM

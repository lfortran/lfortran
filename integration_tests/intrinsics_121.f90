PROGRAM test
    integer(4) :: x = 576897908
    integer(8) :: y = 5

    WRITE (*,*) leadz(0_4)
    if (leadz(0_4) /= 32) error stop

    WRITE (*,*) leadz(5_8)
    if (leadz(5_8) /= 61) error stop

    WRITE (*,*) leadz(x)
    if (leadz(x) /= 2) error stop

    WRITE (*,*) leadz(y)
    if (leadz(y) /= 61) error stop

    WRITE (*,*) leadz(-y)
    if (leadz(-y) /= 0) error stop

    WRITE (*,*) leadz(-x)
    if (leadz(-x) /= 0) error stop

    WRITE (*,*) leadz(-2)
    if (leadz(-2) /= 0) error stop

    WRITE (*,*) leadz(0)
    if (leadz(0) /= 32) error stop

END PROGRAM
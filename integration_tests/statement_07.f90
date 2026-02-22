program statement_07
    implicit double precision (d)

    dadd (d1, d2) = d1 + d2

    da = 1.0d0
    db = 2.0d0
    dc = dadd (da, db)

    if (abs(dc - 3.0d0) > 1d-10) error stop

end program

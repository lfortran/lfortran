program write_25
    implicit none
    real :: a = -0.0044
    real :: b = 0.0044
    character(6) :: s1, s2
    character(7) :: t1, t2

    write(s1, "(e6.1e1)") a
    write(s2, "(e6.1e1)") b
    write(t1, "(e7.1e1)") a
    write(t2, "(e7.1e1)") b

    if (s1 /= "-.4E-2") error stop
    if (s2 /= "0.4E-2") error stop
    if (t1 /= "-0.4E-2") error stop
    if (t2 /= " 0.4E-2") error stop

    print *, "pass"
end program write_25

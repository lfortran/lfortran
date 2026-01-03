double precision function hinit853() result(h)
    double precision :: h1, hmax
    h = 0.1d0
    h1 = 0.1d0
    hmax = 1.d0
    h = min(100*abs(h), h1, hmax)
end function

program intrinsics_55
    double precision :: h
    interface
        double precision function hinit853()
        end function hinit853
    end interface

    h = hinit853()
    if (abs(h-0.10000000000000001) > 1e-8) error stop
    print *, h
end program

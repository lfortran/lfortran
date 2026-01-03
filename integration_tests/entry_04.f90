double precision function mvndfn(dummy)
    double precision :: dummy
    double precision :: mvndnt
    if (abs(dummy - 10.0) > 1.0e-7) error stop
    mvndfn = -12.05
    return

entry mvndnt(dummy)
    mvndnt = 12.05
    if (abs(dummy - 5.0) > 1.0e-7) error stop
    return
end function

program main
    interface
        double precision function mvndfn(dummy)
            double precision :: dummy
        end function

        double precision function mvndnt(dummy)
            double precision :: dummy
        end function
    end interface
    double precision :: dummy
    dummy = 10.0
    if (abs(mvndfn(dummy) - (-12.05)) > 1.0e-7) error stop
    dummy = 5.0
    if (abs(mvndnt(dummy) - 12.05) > 1.0e-7) error stop
end program

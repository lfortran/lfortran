function dnan()
    double precision dnan
    dnan = 0.0d0
end function dnan

subroutine clqmn(x,y)
    implicit double precision (x,y)
    implicit complex*16 (c,z)
    z = dcmplx(x, y)

    xc=abs(z)
    ls=0
    if (dimag(z) == 0.0d0.or.xc < 1.0d0) ls=1
    if(ls /=0 ) error stop
    return
end

program main
    implicit none
    call clqmn(1.0d0, 2.0d0)
end program main
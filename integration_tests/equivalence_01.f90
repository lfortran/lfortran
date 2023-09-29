program equivalence_01
    implicit none
    integer mcheps(4)
    integer minmag(4)
    integer maxmag(4)
    double precision dmach(3)
    equivalence (dmach(1),mcheps(1))
    equivalence (dmach(2),minmag(1))
    equivalence (dmach(3),maxmag(1))
    minmag = 2
    mcheps = 1
    if(minmag(1) /= 1 ) error stop
    if(minmag(2) /= 1 ) error stop
    if(minmag(3) /= 2 ) error stop
    if(minmag(4) /= 2 ) error stop
    if(mcheps(1) /= 1 ) error stop
end
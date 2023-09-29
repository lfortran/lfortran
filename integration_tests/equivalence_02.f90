program equivalence_02
    use iso_c_binding, only: c_loc, c_f_pointer
    implicit none
    integer, pointer :: mcheps(:)
    integer, pointer :: minmag(:)
    integer, pointer :: maxmag(:)
    double precision, target :: dmach(3)
    call c_f_pointer(c_loc(dmach(1)), mcheps, [4])
    call c_f_pointer(c_loc(dmach(2)), minmag, [4])
    call c_f_pointer(c_loc(dmach(3)), maxmag, [4])
    minmag = 2
    mcheps = 1
    if(minmag(1) - 1 > 1e-15) error stop
    if(minmag(2) - 1 > 1e-15) error stop
    if(minmag(3) - 2 > 1e-15) error stop
    if(minmag(4) - 2 > 1e-15) error stop
    if(mcheps(1) - 1 > 1e-15) error stop
end
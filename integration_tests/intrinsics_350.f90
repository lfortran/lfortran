program intrinsics_350
    real(8) :: v(2,3)
    real(8), allocatable :: v_(:)
    real(8) :: v_2(2)
    allocate(v_(size(v,1)))

    v = 2

    v_ = v(:,2)
    v_2 = v(:,2)
    ! output should be the exact same
    print *, sum(1 - spread(v(:,2),dim = 2 , ncopies =23))
    if ( abs(sum(1 - spread(v(:,2),dim = 2 , ncopies =23)) - (-46.0)) > 1e-6 ) error stop
    print *, sum(1 - spread(v_,dim = 2 , ncopies =23))
    if ( abs(sum(1 - spread(v_,dim = 2 , ncopies =23)) - (-46.0)) > 1e-6 ) error stop
    print *, sum(1 - spread(v_2,dim = 2 , ncopies =23))
    if ( abs(sum(1 - spread(v_2,dim = 2 , ncopies =23)) - (-46.0)) > 1e-6 ) error stop
end program main

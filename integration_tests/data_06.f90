program data_06
    integer illin ,ntrep
    common / block_1 / illin, ntrep
    data illin/129/, ntrep/8753/
    if (illin /= 129) error stop
    if (ntrep /= 8753) error stop
    print *, illin, ntrep
end program

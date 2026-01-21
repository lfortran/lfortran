program namelist_merge
    implicit none

    ! Define variables
    logical :: act_w_e
    logical :: act_w_t
    integer :: count
    real :: value

    ! Multiple namelist declarations with the same name should merge
    namelist /act_cli/ act_w_e
    namelist /act_cli/ act_w_t, count
    namelist /act_cli/ value

    ! Initialize variables
    act_w_e = .false.
    act_w_t = .true.
    count = 42
    value = 3.14

    ! Write namelist to file
    open(unit=10, file='namelist_merge.dat', status='replace', form='formatted')
    write(10, nml=act_cli)
    close(10)

    ! Reset variables
    act_w_e = .true.
    act_w_t = .false.
    count = 0
    value = 0.0

    ! Read namelist from file
    open(unit=10, file='namelist_merge.dat', status='old', form='formatted')
    read(10, nml=act_cli)
    close(10)

    ! Verify values
    if (act_w_e) error stop "act_w_e should be false"
    if (.not. act_w_t) error stop "act_w_t should be true"
    if (count /= 42) error stop "count should be 42"
    if (abs(value - 3.14) > 1.0e-5) error stop "value should be 3.14"

    print *, ""
    print *, "Namelist merge test passed!"

end program namelist_merge

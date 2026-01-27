program test_module_namelist
    use config_module
    implicit none

    ! Write module namelist to file
    open(10, file='config.dat', status='replace', form='formatted')
    write(10, nml=config)
    close(10)

    ! Modify values
    param1 = 0
    param2 = 0
    tolerance = 0.0

    ! Read from file
    open(10, file='config.dat', status='old', form='formatted')
    read(10, nml=config)
    close(10)

    ! Verify
    if (param1 /= 10) then
        print *, "Error: param1 =", param1, "expected 10"
        error stop "Module namelist test failed for param1"
    end if

    if (param2 /= 20) then
        print *, "Error: param2 =", param2, "expected 20"
        error stop "Module namelist test failed for param2"
    end if

    if (abs(tolerance - 1.0e-6) > 1.0e-10) then
        print *, "Error: tolerance =", tolerance, "expected 1.0e-6"
        error stop "Module namelist test failed for tolerance"
    end if

    print *, "Module namelist test passed!"

end program test_module_namelist

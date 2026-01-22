program test_allocatable_arrays
    implicit none
    integer, allocatable :: act_name(:)
    real, allocatable :: values(:)
    logical :: temp_log
    integer :: i

    namelist /alloc_nml/ act_name, values, temp_log

    ! Allocate arrays
    allocate(act_name(3))
    allocate(values(4))

    ! Initialize
    act_name = [10, 20, 30]
    values = [1.5, 2.5, 3.5, 4.5]
    temp_log = .true.

    ! Write to file
    open(unit=10, file='namelist_alloc.dat', status='replace', form='formatted')
    write(10, nml=alloc_nml)
    close(10)

    ! Reset values
    act_name = 0
    values = 0.0
    temp_log = .false.

    ! Read from file
    open(unit=10, file='namelist_alloc.dat', status='old', form='formatted')
    read(10, nml=alloc_nml)
    close(10)

    ! Verify act_name
    if (act_name(1) /= 10) then
        print *, "Error: act_name(1) =", act_name(1), "expected 10"
        error stop "Test failed for act_name(1)"
    end if

    if (act_name(2) /= 20) then
        print *, "Error: act_name(2) =", act_name(2), "expected 20"
        error stop "Test failed for act_name(2)"
    end if

    if (act_name(3) /= 30) then
        print *, "Error: act_name(3) =", act_name(3), "expected 30"
        error stop "Test failed for act_name(3)"
    end if

    ! Verify values
    if (abs(values(1) - 1.5) > 1.0e-5) then
        print *, "Error: values(1) =", values(1), "expected 1.5"
        error stop "Test failed for values(1)"
    end if

    if (abs(values(2) - 2.5) > 1.0e-5) then
        print *, "Error: values(2) =", values(2), "expected 2.5"
        error stop "Test failed for values(2)"
    end if

    if (abs(values(3) - 3.5) > 1.0e-5) then
        print *, "Error: values(3) =", values(3), "expected 3.5"
        error stop "Test failed for values(3)"
    end if

    if (abs(values(4) - 4.5) > 1.0e-5) then
        print *, "Error: values(4) =", values(4), "expected 4.5"
        error stop "Test failed for values(4)"
    end if

    ! Verify logical
    if (.not. temp_log) then
        print *, "Error: temp_log =", temp_log, "expected .true."
        error stop "Test failed for temp_log"
    end if

    print *, "Allocatable array namelist test passed!"

    ! Clean up
    deallocate(act_name)
    deallocate(values)

end program test_allocatable_arrays

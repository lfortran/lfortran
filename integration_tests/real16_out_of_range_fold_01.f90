program real16_out_of_range_fold_01
    ! out_of_range of real(16) constants is folded exactly, not through double
    implicit none
    real(16), parameter :: o(6) = [2.0_16**63 - 0.5_16, -2.0_16**63 - 0.75_16, &
        2.0_16**63, -2.0_16**63, 2.0_16**31 - 0.25_16, -2.0_16**31 - 0.75_16]
    logical, parameter :: t8(6) = out_of_range(o, 1_8)
    logical, parameter :: r8(6) = out_of_range(o, 1_8, .true.)
    logical, parameter :: t4(6) = out_of_range(o, 1)
    logical, parameter :: s8 = out_of_range(o(3), 1_8)

    if (any(t8 .neqv. [.false., .false., .true., .false., .false., .false.])) error stop
    if (any(r8 .neqv. [.true., .true., .true., .false., .false., .false.])) error stop
    if (any(t4 .neqv. [.true., .true., .true., .true., .false., .false.])) error stop
    if (.not. s8) error stop
    print *, t8, r8, t4
end program

program iso_fortran_env_03
    use iso_fortran_env, only: logical8, logical16, logical32, logical64, &
        real16, real128, &
        atomic_int_kind, atomic_logical_kind, &
        current_team, initial_team, parent_team, &
        stat_failed_image, stat_unlocked_failed_image
    implicit none

    ! F2023: Logical kind type parameters
    if (logical8 /= 1) error stop
    if (logical16 /= 2) error stop
    if (logical32 /= 4) error stop
    if (logical64 /= 8) error stop

    ! F2023: real16 (half-precision, may not be supported)
    ! real16 == -1 means not supported, which is valid
    if (real16 /= -1 .and. real16 <= 0) error stop

    ! real128 (quad-precision, may not be supported)
    if (real128 /= -1 .and. real128 <= 0) error stop

    ! F2018: Atomic kinds
    if (atomic_int_kind <= 0) error stop
    if (atomic_logical_kind <= 0) error stop

    ! F2018: Team constants (just need to be defined with distinct values)
    if (current_team == initial_team) error stop
    if (current_team == parent_team) error stop
    if (initial_team == parent_team) error stop

    ! F2018: Coarray failure stat constants
    if (stat_failed_image == stat_unlocked_failed_image) error stop
end program iso_fortran_env_03

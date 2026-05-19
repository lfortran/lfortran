program select_type_real128
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    integer, parameter :: r16 = real128
    class(*), allocatable :: x
    logical :: passed

    passed = .false.
    allocate(x, source=1.0)
    select type (x)
    type is (real(r16))
        ! source=1.0 is real(4), so this branch must NOT be taken
        error stop "matched real(real128) for real(4) value"
    class default
        passed = .true.
    end select
    if (.not. passed) error stop "class default was not reached"

    ! Also test that a real(real128) value IS matched by type is (real(r16))
    passed = .false.
    deallocate(x)
    allocate(x, source=1.0_r16)
    select type (x)
    type is (real(r16))
        passed = .true.
    class default
        error stop "did not match real(real128) for real(real128) value"
    end select
    if (.not. passed) error stop "real(real128) branch was not reached"

    print *, "All select_type_real128 tests passed."
end program

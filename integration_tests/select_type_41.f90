program select_type_41
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none

    class(*), allocatable :: value
    allocate(value, source=1.0)

    select type(x => value)
    type is (real(real128))
        ! If real128 is unavailable (=-1), this branch should be ignored.
        error stop
    class default
        if (1 /= 1) error stop
    end select
end program select_type_41
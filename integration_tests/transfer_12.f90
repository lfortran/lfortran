program transfer_12
    ! Test transfer of a derived-type array component to integer(8).
    ! Reproduces a bug where LFortran generated an invalid bitcast
    ! when the source of transfer() was a struct member array.
    use iso_fortran_env, only: int8, int64
    implicit none

    type :: t
        integer(int8) :: data(8)
    end type

    type(t) :: x
    integer(int64) :: result

    x%data = 0_int8
    result = transfer(x%data, 0_int64)
    if (result /= 0_int64) error stop

    x%data = [1_int8, 0_int8, 0_int8, 0_int8, 0_int8, 0_int8, 0_int8, 0_int8]
    result = transfer(x%data, 0_int64)
    if (result /= 1_int64) error stop

    print *, "PASS"
end program

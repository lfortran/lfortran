program cpu_time_02
    use iso_fortran_env, only: real32, real64
    implicit none
    real(real32) :: time32
    real(real64) :: time64

    ! Check that each call writes its output, without requiring a particular
    ! clock value or a positive elapsed time. Negative results are permitted
    ! when the processor cannot provide a meaningful CPU time.
    time32 = -huge(time32)
    call cpu_time(time=time32)
    if (time32 == -huge(time32)) error stop

    time64 = -huge(time64)
    call CPU_TIME(TIME=time64)
    if (time64 == -huge(time64)) error stop

    time32 = -huge(time32)
    call cpu_time(time32)
    if (time32 == -huge(time32)) error stop

    time64 = -huge(time64)
    call cpu_time(time64)
    if (time64 == -huge(time64)) error stop
end program

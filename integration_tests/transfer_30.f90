program transfer_30
    use,intrinsic :: iso_fortran_env, only : int32
    implicit none
    integer(kind=int32) :: o32 = 0
    character(len=1) :: out(4)
    out = transfer(o32, out)
    if (out(1) /= char(0)) error stop
end program transfer_30

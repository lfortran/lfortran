! A user module whose name starts like the name of the modules that hold the
! storage of COMMON blocks is an ordinary module: it is saved to a modfile.
program common_51
    use file_common_block_common_51, only: k
    implicit none
    print *, k
    if (k /= 3) error stop
end program common_51

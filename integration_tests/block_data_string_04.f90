! Test: BLOCK DATA character variable initialization with short literals
! A CHARACTER*N variable in a COMMON block initialized via DATA in BLOCK
! DATA with a literal shorter than N must be blank-padded to length N.
! Without proper padding, bytes from the next member of the common block
! could be read instead of trailing blanks.

program block_data_string_04
    implicit none
    character(6) :: a, b
    common /blk/ a, b

    if (a /= 'ABCDE ') error stop
    if (b /= 'FGHIJK') error stop
    print *, "PASS"
end program

block data bd
    implicit none
    character(6) :: a, b
    common /blk/ a, b
    data a, b /'ABCDE', 'FGHIJK'/
end block data

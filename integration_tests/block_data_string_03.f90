! Test: BLOCK DATA character array initialization via DATA statement
! Verifies that character arrays in COMMON blocks initialized via DATA
! in BLOCK DATA retain their correct values at runtime.

program block_data_string_03
    implicit none
    character(2) :: x(2)
    common /b/ x

    if (x(1) /= 'AB') error stop
    if (x(2) /= 'CD') error stop
    print *, "PASS"
end program

block data
    implicit none
    character(2) :: x(2)
    common /b/ x
    data x /'AB', 'CD'/
end block data

! Test: COMMON block with type aliasing (same storage, different types)
! This tests storage association where the same bytes are viewed differently
program common_17
    implicit none
    double precision :: arr(4)  ! 32 bytes
    common /data/ arr
    arr(1) = 1.0d0
    call sub_int_view()
    print *, "PASS: common_17"
end program

subroutine sub_int_view()
    implicit none
    integer :: iarr(8)  ! 32 bytes (different layout)
    common /data/ iarr
    iarr(1) = 999
end subroutine

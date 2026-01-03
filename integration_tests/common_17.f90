! Test: COMMON block with type aliasing (same storage, different types)
! This tests storage association where the same bytes are viewed differently
program common_17
    implicit none
    double precision :: arr(4)  ! 32 bytes
    double precision :: original_value
    common /data/ arr
    arr(1) = 1.0d0
    original_value = arr(1)
    call sub_int_view()
    ! After sub_int_view modifies the first 4 bytes, arr(1) should be different
    if (arr(1) == original_value) error stop "Type aliasing failed - arr(1) unchanged"
    print *, "PASS: common_17"
end program

subroutine sub_int_view()
    implicit none
    integer :: iarr(8)  ! 32 bytes (different layout)
    common /data/ iarr
    ! Modify first integer (first 4 bytes of arr(1))
    iarr(1) = 999
end subroutine

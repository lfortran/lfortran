program where_scalar_broadcast_01
    ! Test for gh-10461: where construct with scalar broadcast to fixed-size array
    ! Tests that single-element arrays in where blocks don't get index incremented
    implicit none
    
    integer, allocatable :: nz(:)
    integer :: maxnz(1), mx
    integer :: expected
    
    ! Test case: broadcast maxval(nz) to single-element array maxnz
    nz = [480, 120, 1]
    expected = 480
    
    where (nz > 1)
        maxnz = maxval(nz)
    end where
    mx = maxnz(1)
    
    if (mx /= expected) error stop "Test failed: expected 480"
    print *, "Test passed: mx =", mx
    
end program where_scalar_broadcast_01

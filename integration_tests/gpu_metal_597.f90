program gpu_metal_597
    ! Without --descriptor-index-64 a kernel computes element offsets in 32
    ! bits, so a launch that would hand it an array of more than huge(0)
    ! elements must stop with an error that says to recompile with that flag,
    ! instead of running. Here each extent fits in a default integer but
    ! 3*715827883 = 2**31 + 1 elements do not. The loop only writes elements
    ! near the start, which 32-bit offsets reach, so without the check the
    ! program would run to the end and exit 0. The check stops it before
    ! anything is copied, so the 2 GB array is never touched.
    implicit none
    logical(1), allocatable :: b(:,:)
    integer :: i, j, m
    m = 715827883
    allocate(b(3, m))
    do concurrent (j = 1:2, i = 1:3)
        b(i, j) = .true.
    end do
    print *, b(1, 1), b(3, 2)
end program

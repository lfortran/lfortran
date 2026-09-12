program coarrays_47
! Test co_broadcast for derived-type arrays:
!  - allocatable 1-D array of struct (case 1)
!  - non-contiguous section `a(1,:)` of a 2-D allocatable array (case 2)
! Both Exercise the descriptor-based `prif_co_broadcast` path and,
! for the section case, the runtime `is_contiguous` repack that GFortran
! implements via `internal_pack` at the `contiguous` dummy call site.
implicit none

type :: pair_t
    integer :: x
    integer :: y
end type pair_t

integer, parameter :: SRC = 1
integer :: i
logical :: ok

! --- Test 1: allocatable 1-D array of derived type ---
block
    type(pair_t), allocatable :: arr(:)
    integer, parameter :: N1 = 5
    allocate(arr(N1))
    if (this_image() == SRC) then
        do i = 1, N1
            arr(i)%x = i
            arr(i)%y = i*10
        end do
    else
        do i = 1, N1
            arr(i)%x = -1
            arr(i)%y = -1
        end do
    end if
    call co_broadcast(arr, source_image=SRC)
    ok = .true.
    do i = 1, N1
        if (arr(i)%x /= i .or. arr(i)%y /= i*10) ok = .false.
    end do
    if (.not. ok) then
        print *, "Image", this_image(), "failed allocatable derived-type broadcast"
        error stop 1
    end if
    if (this_image() == SRC) print *, "OK allocatable derived-type broadcast"
end block
! --- Test 2: non-contiguous row section a(1,:) of a 2-D allocatable array ---
block
    type(pair_t), allocatable :: mat(:,:)
    integer, parameter :: N2 = 5
    integer :: j
    
    allocate(mat(N2, N2))
    
    ! Initialize the ENTIRE matrix on ALL images with sentinels
    do j = 1, N2
        do i = 1, N2
            mat(i,j)%x = -999
            mat(i,j)%y = -999
        end do
    end do

    ! Set up the target row on the source image, and dummy data on receivers
    if (this_image() == SRC) then
        do i = 1, N2
            mat(1,i)%x = i
            mat(1,i)%y = i*100
        end do
    else
        do i = 1, N2
            mat(1,i)%x = -1
            mat(1,i)%y = -1
        end do
    end if
    
    ! `mat(1,:)` is a row of a column-major 2-D array, i.e. strided
    ! with stride = N2, hence non-contiguous. GFortran packs it via
    ! `internal_pack` at the `contiguous` dummy call inside
    ! `prif_co_broadcast -> contiguous_co_broadcast`.
    call co_broadcast(mat(1,:), source_image=SRC)
    
    ok = .true.
    ! 1. Validate the broadcasted row arrived safely
    do i = 1, N2
        if (mat(1,i)%x /= i .or. mat(1,i)%y /= i*100) ok = .false.
    end do
    
    ! 2. Validate that NO other memory was corrupted (the GDB bug!)
    do j = 1, N2
        do i = 2, N2
            if (mat(i,j)%x /= -999 .or. mat(i,j)%y /= -999) ok = .false.
        end do
    end do

    if (.not. ok) then
        print *, "Image", this_image(), "failed non-contiguous section broadcast"
        error stop 2
    end if
    if (this_image() == SRC) print *, "OK non-contiguous section broadcast"
end block

if (this_image() == SRC) print *, "coarrays_47 passed"

end program

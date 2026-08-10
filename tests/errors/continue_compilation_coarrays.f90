program continue_compilation_coarrays

    ! Test: coindex notation on non-coarray variable should produce error
    integer :: x
    x = x[1]

    ! Test: corank mismatch should produce error
    integer :: B[3,*]
    B[1] = 5

    type :: t
        integer :: x
        integer :: y[3,*]
    end type t
    type(t) :: s
    ! Should error: x is not a coarray
    s%x = s%x[1]
    
    ! Corank = 2, but only 1 coindex provided → ERROR
    s%y[1] = 5

    integer :: z[*], a
    a = z[1:5:2]

    real :: cod[4,*]
    print *,cod[5,1]

    ! ALLOCATE coarray-spec: multiple `*` cobounds -> ERROR
    integer, allocatable :: acoarr[:,:]
    allocate(acoarr[*, *])

    ! ALLOCATE coarray-spec: last cobound must be `*` -> ERROR
    integer, allocatable :: acoarr2[:,:]
    allocate(acoarr2[2, 2])

    ! Assumed codimension `*` must be the last codimension → ERROR
    integer :: w1[*, *]

    ! Assumed codimension `*` not in the last position → ERROR
    integer :: w2[*, 2]

    ! CODIMENSION attribute: last cobound must be `*` → ERROR
    integer, codimension[3, 2] :: w3

    ! last ucobound must be '*' -> ERROR
    real :: w4[4]

    ! last ucobound must be '*' -> ERROR
    real :: w5[2:4]

    ! last ucobound must be '*' -> ERROR
    real, codimension[5] :: w6

    ! last ucobound must be '*' -> ERROR
    real, codimension[2:5] :: w7

    ! C828: a nonallocatable coarray may not have a deferred coshape -> ERROR
    integer :: c828a[:]
    integer :: c828b[:,*]
    integer :: c828c[2,:,*]

    ! C827: an allocatable coarray may not have an explicit coshape -> ERROR
    integer, allocatable :: c827a[*]
    integer, allocatable :: c827b[:,*]
    integer, allocatable :: c827c[:,4,:]
    integer, codimension[2,10,*], allocatable :: c827d

end program continue_compilation_coarrays
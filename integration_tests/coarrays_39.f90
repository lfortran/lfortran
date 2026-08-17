program coarrays_39
    implicit none

    ! --- Coarrays of varying corank, to exercise the COARRAY argument ---
    integer :: s[*]              ! scalar coarray, corank 1
    integer :: v1(5)[*]          ! array coarray, corank 1
    integer :: v2[2,*]           ! corank 2
    integer :: v3[2,1,*]         ! corank 3
    real    :: r3[2,1,*]         ! different type, corank 3
    logical, allocatable :: la[:]      ! allocatable scalar coarray, corank 1
    integer, allocatable :: la2(:)[:,:]! allocatable coarray, corank 2

    integer :: n
    integer, parameter :: k4 = kind(0)
    integer, parameter :: k8 = selected_int_kind(15)

    n = num_images()
    if (n /= 6) then
        print *, 'This test assumes num_images() == 6; got', n
        error stop
    end if

    !======================================================
    ! 1. COARRAY argument: scalar coarray, corank 1
    !======================================================
    if (size(coshape(s)) /= 1) then
        print *, 'FAIL 1a: size(coshape(s)) =', size(coshape(s))
        error stop
    end if
    if (any(coshape(s) /= [6])) then      ! extent = ceil(6/1) = 6
        print *, 'FAIL 1b: coshape(s) =', coshape(s)
        error stop
    end if

    !======================================================
    ! 2. COARRAY argument: array coarray, corank 1
    !    (coshape must ignore the normal array shape, size(v1)=5,
    !     and report only the codimension extents)
    !======================================================
    if (any(coshape(v1) /= [6])) then
        print *, 'FAIL 2: coshape(v1) =', coshape(v1)
        error stop
    end if

    !======================================================
    ! 3. COARRAY argument: corank 2, explicit + assumed-size codim
    !======================================================
    if (any(coshape(v2) /= [2, 3])) then   ! ceil(6/2) = 3
        print *, 'FAIL 3: coshape(v2) =', coshape(v2)
        error stop
    end if

    !======================================================
    ! 4. COARRAY argument: corank 3
    !======================================================
    if (any(coshape(v3) /= [2, 1, 3])) then  ! ceil(6/(2*1)) = 3
        print *, 'FAIL 4: coshape(v3) =', coshape(v3)
        error stop
    end if

    !======================================================
    ! 5. COARRAY argument: type-independence
    !    coshape does not depend on the dynamic type of COARRAY,
    !    only its codimensions. r3 has same codim decl as v3.
    !======================================================
    if (any(coshape(r3) /= coshape(v3))) then
        print *, 'FAIL 5: coshape(r3) =', coshape(r3), &
                  ' coshape(v3) =', coshape(v3)
        error stop
    end if

    !======================================================
    ! 6. COARRAY argument: allocatable coarrays
    !    (must be allocated before coshape is well-defined)
    !======================================================
    allocate(la[*])
    if (any(coshape(la) /= [6])) then
        print *, 'FAIL 6a: coshape(la) =', coshape(la)
        error stop
    end if
    deallocate(la)

    allocate(la2(3)[2,*])
    if (any(coshape(la2) /= [2, 3])) then   ! ceil(6/2)=3
        print *, 'FAIL 6b: coshape(la2) =', coshape(la2)
        error stop
    end if
    deallocate(la2)

    !======================================================
    ! 7. KIND argument: absent -> default integer kind
    !======================================================
    if (kind(coshape(v3)) /= k4) then
        print *, 'FAIL 7: kind(coshape(v3)) =', kind(coshape(v3)), &
                  ' expected default kind', k4
        error stop
    end if

    !======================================================
    ! 8. KIND argument: present -> result has that kind
    !======================================================
    if (kind(coshape(v3, kind=k8)) /= k8) then
        print *, 'FAIL 8: kind =', kind(coshape(v3, kind=k8)), &
                  ' expected', k8
        error stop
    end if
    ! values must be unaffected by kind choice
    if (any(coshape(v3, kind=k8) /= [2, 1, 3])) then
        print *, 'FAIL 8b: coshape(v3,kind=k8) =', coshape(v3, kind=k8)
        error stop
    end if

    !======================================================
    ! 9. KIND argument passed positionally vs. by keyword
    !======================================================
    if (any(coshape(v3, k8) /= coshape(v3, kind=k8))) then
        print *, 'FAIL 9: positional/keyword KIND mismatch'
        error stop
    end if

    !======================================================
    ! 10. Result rank/size: coshape always rank-1, size = corank
    !======================================================
    if (rank(coshape(s))  /= 1 .or. size(coshape(s))  /= 1)  then
        print *, 'FAIL 10a'; error stop
    end if
    if (rank(coshape(v2)) /= 1 .or. size(coshape(v2)) /= 2) then
        print *, 'FAIL 10b'; error stop
    end if
    if (rank(coshape(v3)) /= 1 .or. size(coshape(v3)) /= 3) then
        print *, 'FAIL 10c'; error stop
    end if

end program coarrays_39
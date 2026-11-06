program gpu_metal_728
    ! The logical reductions .and., .or., .eqv. and .neqv. of an offloaded
    ! do concurrent loop (lfortran/lfortran#16725). Every result is folded
    ! into the value the variable held before the loop, so the loops are
    ! run from both starting values of each operator, and over no
    ! iterations at all, where the result is the starting value. The
    ! accumulators are of the default kind, then logical(1) and logical(2).
    implicit none
    ! More iterations than the threads of a launch with a reduction.
    integer, parameter :: nw = 600011
    integer :: i, n
    logical :: a, o, e, x, ea, eo, ee, ex
    logical :: flags(20)
    logical, allocatable :: w(:)
    logical(1) :: a1, o1, e1, x1
    logical(2) :: a2, o2, e2, x2

    ! The program of the issue.
    a = .true.; o = .false.; e = .true.; x = .false.
    do concurrent (i = 1:20) reduce(.and.:a) reduce(.or.:o) &
            reduce(.eqv.:e) reduce(.neqv.:x)
        a = a .and. (i < 30)
        o = o .or. (i == 17)
        e = e .eqv. (i /= 5)
        x = x .neqv. (i == 5)
    end do
    if (.not. a) error stop 1
    if (.not. o) error stop 2
    if (e) error stop 3
    if (.not. x) error stop 4

    ! The other starting value of each: .and. from .false. stays .false.,
    ! .or. from .true. stays .true., and .eqv. and .neqv. invert.
    a = .false.; o = .true.; e = .false.; x = .true.
    do concurrent (i = 1:20) reduce(.and.:a) reduce(.or.:o) &
            reduce(.eqv.:e) reduce(.neqv.:x)
        a = a .and. (i < 30)
        o = o .or. (i == 17)
        e = e .eqv. (i /= 5)
        x = x .neqv. (i == 5)
    end do
    if (a) error stop 5
    if (.not. o) error stop 6
    if (.not. e) error stop 7
    if (x) error stop 8

    ! One iteration decides .and. and .or.; none of them sets .or.
    a = .true.; o = .false.
    do concurrent (i = 1:20) reduce(.and.:a) reduce(.or.:o)
        a = a .and. (i /= 13)
        o = o .or. (i > 20)
    end do
    if (a) error stop 9
    if (o) error stop 10

    ! Values read from an array, and accumulators only some iterations
    ! reach: the others leave the identity in their slot.
    do i = 1, 20
        flags(i) = mod(i, 3) == 0
    end do
    a = .true.; o = .false.; e = .true.; x = .false.
    do concurrent (i = 1:20) reduce(.and.:a) reduce(.or.:o) &
            reduce(.eqv.:e) reduce(.neqv.:x)
        if (mod(i, 2) == 0) then
            a = a .and. flags(i)
            o = o .or. flags(i)
            e = e .eqv. flags(i)
            x = x .neqv. flags(i)
        end if
    end do
    ! The even i: 2, 4, ..., 20; flags(i) holds for 6, 12 and 18 only.
    if (a) error stop 11
    if (.not. o) error stop 12
    ! .eqv. over 7 .false. values: .true. .eqv. .false. seven times.
    if (e) error stop 13
    ! .neqv. over 3 .true. values.
    if (.not. x) error stop 14

    ! No iterations: every variable keeps its value.
    n = 0
    a = .false.; o = .true.; e = .false.; x = .true.
    do concurrent (i = 1:n) reduce(.and.:a) reduce(.or.:o) &
            reduce(.eqv.:e) reduce(.neqv.:x)
        a = .true.
        o = .false.
        e = .true.
        x = .false.
    end do
    if (a .or. .not. o .or. e .or. .not. x) error stop 15

    ! logical(1) accumulators.
    a1 = .true.; o1 = .false.; e1 = .true.; x1 = .false.
    do concurrent (i = 1:20) reduce(.and.:a1) reduce(.or.:o1) &
            reduce(.eqv.:e1) reduce(.neqv.:x1)
        a1 = a1 .and. (i < 30)
        o1 = o1 .or. (i == 17)
        e1 = e1 .eqv. (i /= 5)
        x1 = x1 .neqv. (i == 5)
    end do
    if (.not. a1) error stop 16
    if (.not. o1) error stop 17
    if (e1) error stop 18
    if (.not. x1) error stop 19

    ! logical(2) accumulators, from the other starting values.
    a2 = .false.; o2 = .true.; e2 = .false.; x2 = .true.
    do concurrent (i = 1:21) reduce(.and.:a2) reduce(.or.:o2) &
            reduce(.eqv.:e2) reduce(.neqv.:x2)
        a2 = a2 .and. (i < 30)
        o2 = o2 .or. (i == 17)
        e2 = e2 .eqv. (mod(i, 2) == 0)
        x2 = x2 .neqv. (mod(i, 2) == 0)
    end do
    if (a2) error stop 20
    if (.not. o2) error stop 21
    ! 11 odd i give .false.: .eqv. inverts 11 times.
    if (.not. e2) error stop 22
    ! 10 even i give .true.: .neqv. inverts 10 times.
    if (.not. x2) error stop 23

    ! A cycle ends some iterations early, so the threads accumulate in
    ! their slots directly rather than in a local variable.
    a = .true.; o = .false.; e = .true.; x = .false.
    do concurrent (i = 1:20) reduce(.and.:a) reduce(.or.:o) &
            reduce(.eqv.:e) reduce(.neqv.:x)
        if (mod(i, 4) == 0) cycle
        a = a .and. (i /= 7)
        o = o .or. (i == 8)
        e = e .eqv. (i > 9)
        x = x .neqv. (i > 10)
    end do
    if (a) error stop 24
    ! i = 8 is skipped.
    if (o) error stop 25
    ! 7 of the iterations not skipped give .false.: 1, 2, 3, 5, 6, 7, 9.
    if (e) error stop 26
    ! 7 give .true.: 11, 13, 14, 15, 17, 18, 19.
    if (.not. x) error stop 27

    ! More iterations than threads: each thread accumulates several of
    ! them before it combines its result into its slot. Compared with the
    ! same reduction run serially on the host.
    allocate(w(nw))
    do i = 1, nw
        w(i) = mod(i, 7) /= 3
    end do
    a = .true.; o = .false.; e = .false.; x = .true.
    ea = a; eo = o; ee = e; ex = x
    do concurrent (i = 1:nw) reduce(.and.:a) reduce(.or.:o) &
            reduce(.eqv.:e) reduce(.neqv.:x)
        a = a .and. w(i)
        o = o .or. .not. w(i)
        e = e .eqv. w(i)
        x = x .neqv. w(i)
    end do
    do i = 1, nw
        ea = ea .and. w(i)
        eo = eo .or. .not. w(i)
        ee = ee .eqv. w(i)
        ex = ex .neqv. w(i)
    end do
    if (a .neqv. ea) error stop 28
    if (o .neqv. eo) error stop 29
    if (e .neqv. ee) error stop 30
    if (x .neqv. ex) error stop 31
    ! 85716 .false. values and 514295 .true. ones.
    if (a .or. .not. o .or. e .or. x) error stop 32

    print *, "ok"
end program

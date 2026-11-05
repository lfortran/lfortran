program gpu_metal_730
    ! The logical operators .eqv. and .neqv. in an offloaded do concurrent
    ! loop without a reduction (lfortran/lfortran#16630): on scalars, on
    ! array elements, and as the condition of merge.
    implicit none
    integer :: b(4), r(4), i
    logical :: k, m(4), p(4), q(4)
    logical(1) :: s(4)

    b = [-16, 1, -1, 64]
    k = .true.
    do concurrent (i = 1:4)
        m(i) = (b(i) < 0) .neqv. k
        r(i) = merge(1, 0, m(i) .eqv. k)
    end do
    if (any(m .neqv. [.false., .true., .false., .true.])) error stop 1
    if (any(r /= [0, 1, 0, 1])) error stop 2

    p = [.true., .true., .false., .false.]
    q = [.true., .false., .true., .false.]
    do concurrent (i = 1:4)
        m(i) = p(i) .eqv. q(i)
        s(i) = p(i) .neqv. q(i)
    end do
    if (any(m .neqv. [.true., .false., .false., .true.])) error stop 3
    if (any(s .neqv. [.false., .true., .true., .false.])) error stop 4

    print *, "ok"
end program

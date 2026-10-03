program parameter_17
    ! Regression test: a PARAMETER initializer that references another
    ! PARAMETER, used in a scope that also contains a COMMON block.
    ! Previously, the COMMON-block processing would recompute the
    ! parameter's m_dependencies from m_type and m_value only, dropping
    ! the dependency on `nmax` carried in m_symbolic_value, and the
    ! ASR verifier would fail with:
    !   Variable kdmax depends on nmax but isn't found in its
    !   dependency list.
    implicit none
    integer, parameter :: nmax = 132
    integer, parameter :: kdmax = nmax + (nmax + 1) / 4
    integer :: a, b
    common /blk/ a, b
    a = 1
    b = 2
    if (kdmax /= 165) error stop
    if (a /= 1 .or. b /= 2) error stop
    print *, kdmax
end program

subroutine sqrt16(m, n, nrhs, work, ldwork, work2, result)
    implicit none

    integer, intent(in) :: m, n, nrhs, ldwork
    real, intent(inout) :: work(ldwork, *)
    real, intent(inout) :: work2(*)
    real, intent(out) :: result

    work2(1) = 2.0
    result = work2(1)
end subroutine sqrt16

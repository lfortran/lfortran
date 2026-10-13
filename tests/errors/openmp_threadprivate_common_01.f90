! An OpenMP THREADPRIVATE directive that names a common block is not
! supported yet; it must be reported, not ignored.
subroutine openmp_threadprivate_common_01()
    implicit none
    integer :: t
    common /otc_blk/ t
    !$omp threadprivate(/otc_blk/)
    t = 1
end subroutine openmp_threadprivate_common_01

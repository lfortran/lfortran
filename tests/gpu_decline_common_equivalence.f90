! A variable that EQUIVALENCE associates with a common block, read by a
! procedure that a parallel loop offloaded to the gpu calls: an error.
module gpu_decline_common_equivalence_mod
    implicit none
contains
    real function from_equivalence(k)
        integer, intent(in) :: k
        real :: w(4), e(4)
        common /gpu_decline_common_e/ w
        equivalence (w, e)
        from_equivalence = e(k)
    end function
end module

program gpu_decline_common_equivalence
    use gpu_decline_common_equivalence_mod
    implicit none
    real :: d(4), u(4)
    common /gpu_decline_common_e/ d
    integer :: k
    d = [1, 2, 3, 4]
    !$omp parallel do
    do k = 1, 4
        u(k) = from_equivalence(k)
    end do
    !$omp end parallel do
    print *, u
    if (any(u /= d)) error stop
end program

subroutine a1(n, A, B)
integer, intent(in) :: n
real, intent(in) :: A(n)
real, intent(out) :: B(n)
integer :: i
!$OMP PARALLEL DO
do i = 2, N
    B(i) = (A(i) + A(i-1)) / 2
end do
!$OMP END PARALLEL DO
end subroutine

subroutine parallel_sum(n, a)
    !$omp parallel private(partial_sum) shared(total_sum)
    partial_sum = 0
    total_sum = 0

    !$omp do
        do i = 1, n
            partial_sum = partial_sum + a(i)
        end do
    !$omp end do

    !$omp critical
        total_sum = total_sum + partial_sum
    !$omp end critical
!$omp end parallel
end subroutine

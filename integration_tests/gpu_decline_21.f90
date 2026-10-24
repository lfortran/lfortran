! gpu decline reason: NamedCycleInNestedConcurrent
! ASR does not keep the name of a do concurrent, so a named cycle inside a
! do concurrent nested in an offloaded one cannot be told to be of either,
! and the loop is not offloaded.
! Not registered for llvm: plain LLVM cannot find a do concurrent by name.
program gpu_decline_21
implicit none
integer :: p(4, 3), i, j, k
p = 0
do concurrent (i = 1:4)
    inner: do concurrent (j = 1:3)
        do k = 1, 2
            if (j == 2) cycle inner
            p(i, j) = p(i, j) + k
        end do
    end do inner
end do
print *, p
if (any(p(:, 2) /= 0)) error stop
if (any(p(:, 1) /= 3) .or. any(p(:, 3) /= 3)) error stop
end program

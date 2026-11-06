program gpu_workspace_guard_02
    implicit none
    real :: y(32)
    integer :: flag
    do flag = 0, 1
        call evaluate(y, 17, flag == 1)
        if (flag == 0 .and. any(y /= 51)) error stop "negative guard"
        if (flag == 1 .and. any(y /= 7)) error stop "unrelated work"
    end do
    print *, "workspace polarity: PASS"
contains
    subroutine evaluate(y, n, active)
        integer, intent(in) :: n
        logical, intent(in) :: active
        real, intent(out) :: y(:)
        integer :: j
        do concurrent(j=1:size(y))
            block
                real :: work(n)
                integer :: d
                if (.not. active) then
                    do d = 1, n
                        work(d) = 3
                    end do
                end if
                if (active) then
                    y(j) = 7
                else
                    y(j) = sum(work)
                end if
            end block
        end do
    end subroutine
end program

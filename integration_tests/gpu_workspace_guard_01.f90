program gpu_workspace_guard_01
    implicit none
    integer :: flag, n, j
    real :: x(32), y(32)
    integer :: bounds(3,32)

    do flag = 0, 1
        x = 2
        call uniform(x, y, bounds, 17, flag == 1)
        do j = 1, 32
            if (any(bounds(:,j) /= [17, -2, 14])) error stop "bounds"
            if (flag == 0 .and. y(j) /= 2) error stop "inactive"
            if (flag == 1 .and. y(j) /= 34) error stop "active"
        end do
        call nested(y, 17, flag == 1, .false.)
        if (any(y /= 3)) error stop "nested inactive"
        call nested(y, 17, flag == 1, .true.)
        if (flag == 0 .and. any(y /= 3)) error stop "nested outer"
        if (flag == 1 .and. any(y /= 68)) error stop "nested active"
    end do
    call varying(y, 17)
    do j = 1, 32
        if (mod(j,2) == 0) then
            if (y(j) /= 85) error stop "varying active"
        else
            if (y(j) /= 7) error stop "varying inactive"
        end if
    end do
    ! Division by zero in the inner test must not be hoisted.
    call guarded_division(y, 17, .false., 0)
    if (any(y /= 9)) error stop "unreached division"
    call guarded_division(y, 17, .true., 2)
    if (any(y /= 102)) error stop "reached division"
    do n = 0, 1
        call uniform(x, y, bounds, n, .true.)
        if (any(y /= real(2*n))) error stop "empty extent"
    end do
    print *, "workspace guards: PASS"
contains
    subroutine uniform(x, y, bounds, n, active)
        integer, intent(in) :: n
        logical, intent(in) :: active
        real, intent(in) :: x(:)
        real, intent(out) :: y(:)
        integer, intent(out) :: bounds(:,:)
        integer :: j
        do concurrent(j=1:size(y))
            block
                real :: work(-2:n-3)
                integer :: d
                bounds(1,j) = size(work)
                bounds(2,j) = lbound(work,1)
                bounds(3,j) = ubound(work,1)
                if (active) then
                    do d = -2, n-3
                        work(d) = x(j)
                    end do
                    y(j) = sum(work)
                else
                    y(j) = x(j)
                end if
            end block
        end do
    end subroutine
    subroutine nested(y, n, outer, inner)
        integer, intent(in) :: n
        logical, intent(in) :: outer, inner
        real, intent(out) :: y(:)
        integer :: j
        do concurrent(j=1:size(y))
            block
                real :: work(n)
                integer :: d
                y(j) = 3
                if (outer) then
                    if (inner) then
                        do d = 1, n
                            work(d) = 4
                        end do
                        y(j) = sum(work)
                    end if
                end if
            end block
        end do
    end subroutine
    subroutine varying(y, n)
        integer, intent(in) :: n
        real, intent(out) :: y(:)
        integer :: j
        do concurrent(j=1:size(y))
            block
                real :: work(n)
                integer :: d
                y(j) = 7
                if (mod(j,2) == 0) then
                    do d = 1, n
                        work(d) = 5
                    end do
                    y(j) = sum(work)
                end if
            end block
        end do
    end subroutine
    subroutine guarded_division(y, n, active, divisor)
        integer, intent(in) :: n, divisor
        logical, intent(in) :: active
        real, intent(out) :: y(:)
        integer :: j
        do concurrent(j=1:size(y))
            block
                real :: work(n)
                integer :: d
                y(j) = 9
                if (active) then
                    if (10 / divisor > 0) then
                        do d = 1, n
                            work(d) = 6
                        end do
                        y(j) = sum(work)
                    end if
                end if
            end block
        end do
    end subroutine
end program

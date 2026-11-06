program gpu_workspace_guard_03
    implicit none
    real :: y(3)
    integer :: flag
    do flag = 0, 1
        call nested(4, flag == 1, y)
        if (flag == 0 .and. any(y /= 10)) error stop "nested active"
        if (flag == 1 .and. any(y /= 7)) error stop "nested inactive"
        call inherited(4, flag == 1, y)
        if (flag == 0 .and. any(y /= 7)) error stop "inherited inactive"
        if (flag == 1 .and. any(y /= 10)) error stop "inherited active"
        call outside_use(4, flag == 1, y)
        if (flag == 0 .and. any(y /= 10)) error stop "outside active"
        if (flag == 1 .and. any(y /= 7)) error stop "outside inactive"
        call bound_use(4, flag == 1, y)
        if (flag == 0 .and. any(y /= 10)) error stop "bound active"
        if (flag == 1 .and. any(y /= 7)) error stop "bound inactive"
    end do
    call varying(4, y)
    if (any(y /= [7.0, 10.0, 7.0])) error stop "private predicate"
    call nested(0, .false., y)
    if (any(y /= 0)) error stop "empty active"
    call nested(0, .true., y)
    if (any(y /= 7)) error stop "empty inactive"
    print *, "nested workspace guards: PASS"
contains
    pure subroutine nested(k, vector, y)
        integer, intent(in) :: k
        logical, intent(in) :: vector
        real, intent(out) :: y(3)
        integer :: n, d
        do concurrent (n = 1:3)
            block
                real :: nested_work(k)
                if (size(nested_work) /= k) error stop "shape"
                do concurrent (d = 1:k)
                    if (.not. vector) nested_work(d) = real(d)
                end do
                if (vector) then
                    y(n) = 7
                else
                    y(n) = sum(nested_work)
                end if
            end block
        end do
    end subroutine

    pure subroutine inherited(k, active, y)
        integer, intent(in) :: k
        logical, intent(in) :: active
        real, intent(out) :: y(3)
        integer :: n, d
        do concurrent (n = 1:3)
            block
                real :: inherited_work(k)
                y(n) = 7
                if (active) then
                    do concurrent (d = 1:k)
                        if (d > 0) inherited_work(d) = real(d)
                    end do
                    y(n) = sum(inherited_work)
                end if
            end block
        end do
    end subroutine

    pure subroutine varying(k, y)
        integer, intent(in) :: k
        real, intent(out) :: y(3)
        integer :: n, d
        do concurrent (n = 1:3)
            block
                real :: varying_work(k)
                logical :: vector
                vector = modulo(n, 2) == 1
                do concurrent (d = 1:k)
                    if (.not. vector) varying_work(d) = real(d)
                end do
                if (vector) then
                    y(n) = 7
                else
                    y(n) = sum(varying_work)
                end if
            end block
        end do
    end subroutine

    pure subroutine outside_use(k, vector, y)
        integer, intent(in) :: k
        logical, intent(in) :: vector
        real, intent(out) :: y(3)
        integer :: n, d
        do concurrent (n = 1:3)
            block
                real :: outside_work(k)
                do concurrent (d = 1:k)
                    outside_work(d) = real(d)
                end do
                if (vector) then
                    y(n) = 7
                else
                    y(n) = sum(outside_work)
                end if
            end block
        end do
    end subroutine

    pure subroutine bound_use(k, vector, y)
        integer, intent(in) :: k
        logical, intent(in) :: vector
        real, intent(out) :: y(3)
        integer :: n, d
        do concurrent (n = 1:3)
            block
                real :: bound_work(k)
                bound_work(1) = real(k)
                do concurrent (d = 1:int(bound_work(1)))
                    if (.not. vector) bound_work(d) = real(d)
                end do
                if (vector) then
                    y(n) = 7
                else
                    y(n) = sum(bound_work)
                end if
            end block
        end do
    end subroutine
end program

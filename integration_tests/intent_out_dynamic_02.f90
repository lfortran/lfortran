program intent_out_dynamic_02
    use intent_out_dynamic_base
    use intent_out_dynamic_extension
    implicit none
    type(leaf) :: x(-2:1, 3:4)
    integer :: i, j
    do j = 3, 4
        do i = -2, 1
            allocate(x(i,j)%data(1), x(i,j)%payload)
            x(i,j)%added = 99
            x(i,j)%parts%n = 99
        end do
    end do
    final_calls = 0
    call forward()
    if (final_calls /= 0) error stop 1
    call forward(x(-2:1:2, :))
    if (final_calls /= 4) error stop 2
    do j = 3, 4
        do i = -2, 1
            if (mod(i, 2) == 0) then
                if (allocated(x(i,j)%data) .or. allocated(x(i,j)%payload)) error stop 3
                if (x(i,j)%added /= 9 .or. any(x(i,j)%parts%n /= 5)) error stop 4
            else
                if (.not. allocated(x(i,j)%data) .or. .not. allocated(x(i,j)%payload)) error stop 5
                if (x(i,j)%added /= 99 .or. any(x(i,j)%parts%n /= 99)) error stop 6
            end if
        end do
    end do
    call reset_array(x)
    if (final_calls /= 8) error stop 7
    if (any(x%added /= 9)) error stop 8
end program

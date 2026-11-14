program intent_out_dynamic_01
    use intent_out_dynamic_base
    use intent_out_dynamic_extension
    implicit none
    type(leaf) :: x
    character(len=1), target :: target_char = 'x'
    integer :: i

    do i = 1, 3
        allocate(x%data(2), x%payload, x%parts(1)%data(3))
        x%inherited = 99
        x%added = 99
        x%more = 99.0
        x%parts%n = 99
        x%text = 'zzzz'
        x%labels = 'zz'
        x%pointer_char => target_char
        final_calls = 0
        if (i == 1) then
            call reset(x)
        else
            call reset_any(x)
        end if
        if (allocated(x%data) .or. allocated(x%payload)) error stop 1
        if (allocated(x%parts(1)%data)) error stop 2
        if (final_calls /= 1) error stop 3
        if (x%inherited /= 4 .or. x%added /= 9 .or. x%more /= 3.0) error stop 4
        if (any(x%parts%n /= 5)) error stop 5
        if (x%text /= 'abcd' .or. any(x%labels /= ['ab', 'cd'])) error stop 6
        if (associated(x%pointer_char)) error stop 7
        if (target_char /= 'x') error stop 8
    end do
    ! An intrinsic dynamic type has no components to reset.
    call reset_any(i)
    i = 1
end program

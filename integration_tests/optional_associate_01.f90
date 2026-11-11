module optional_associate_01_m
    implicit none
    type :: base_t
        integer :: v = 0
    end type
    type, extends(base_t) :: child_t
    end type
contains
    subroutine done(ie, expected)
        integer, intent(in), optional :: ie
        logical, intent(in) :: expected
        if (present(ie) .neqv. expected) error stop "wrong presence"
        if (present(ie)) then
            if (ie /= 7) error stop "wrong value"
        end if
    end subroutine

    subroutine in_associate(iv, ie, expected)
        integer, intent(in) :: iv
        integer, intent(in), optional :: ie
        logical, intent(in) :: expected
        associate (k => iv)
            call done(ie, expected)
        end associate
    end subroutine

    subroutine nested_associate(iv, ie, expected)
        integer, intent(in) :: iv
        integer, intent(in), optional :: ie
        logical, intent(in) :: expected
        associate (k => iv)
            block
                associate (m => k)
                    call done(ie, expected)
                end associate
            end block
        end associate
    end subroutine

    subroutine in_select_type(c, ie, expected)
        class(base_t), intent(in) :: c
        integer, intent(in), optional :: ie
        logical, intent(in) :: expected
        select type (c)
        type is (child_t)
            call done(ie, expected)
        class default
            call done(ie, expected)
        end select
    end subroutine
end module

program optional_associate_01
    use optional_associate_01_m
    implicit none
    type(child_t) :: c
    call in_associate(3, expected=.false.)
    call in_associate(3, 7, .true.)
    call nested_associate(3, expected=.false.)
    call nested_associate(3, 7, .true.)
    call in_select_type(c, expected=.false.)
    call in_select_type(c, 7, .true.)
end program

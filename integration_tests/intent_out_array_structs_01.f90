module m
    type :: regex_token
        integer :: id
        character(len=:), allocatable :: ccl
    end type

    type :: regex_pattern
        type(regex_token), dimension(5) :: pattern
    end type
contains
    subroutine parse_pattern(this)
        type(regex_pattern), intent(out) :: this
        this%pattern(1)%id = 1
    end subroutine
end module

program intent_out_array_structs
    use m
    type(regex_pattern) :: p
    call parse_pattern(p)
    if (p%pattern(1)%id /= 1) error stop
end program

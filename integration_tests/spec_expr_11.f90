module spec_expr_11_mod
    implicit none
    character(*), parameter :: text = 'abcdeedcba'
    character(len(text)/2) :: module_text
    character(len(module_text)) :: module_inquiry
    character(1) :: module_override*(len(module_text))
contains
    pure function half(s) result(t)
        character(*), intent(in) :: s
        character(len(s)/2) :: t
        t = s
    end function

    pure function fixed(s) result(t)
        character(*), intent(in) :: s
        character(5) :: t
        t = s
    end function

    function check_function(s) result(t)
        character(*), intent(in) :: s
        character(len(half(s))) :: t
        t = half(s)
    end function

    subroutine check_subroutine(s)
        character(*), intent(in) :: s
        character(len(half(s))) :: t
        character :: old*(len(half(s)))
        character(1) :: overridden*(len(half(s)))
        t = half(s)
        old = t
        overridden = t
        if (len(t) /= 5 .or. t /= 'abcde' .or. old /= t) error stop
        if (len(overridden) /= 5 .or. overridden /= t) error stop
        block
            character(len(half(s))) :: b
            b = half(s)
            if (len(b) /= 5 .or. b /= t) error stop
        end block
    end subroutine
end module

program spec_expr_11
    use spec_expr_11_mod
    implicit none
    character(len(text)/2) :: main_text
    character(len(main_text)) :: main_inquiry
    character(len(fixed(text))) :: fixed_inquiry
    integer :: a(5)
    character(size(a)) :: size_inquiry
    character(ubound(a,1)) :: upper_inquiry
    character(lbound(a,1)) :: lower_inquiry
    character(rank(a)) :: rank_inquiry
    interface
        function external_half(s) result(t)
            import half
            character(*), intent(in) :: s
            character(len(half(s))) :: t
        end function
    end interface
    interface generic_length
        pure integer function spec_expr_11_length(n)
            integer, intent(in) :: n
        end function
    end interface
    main_text = 'abcde'
    main_inquiry = main_text
    module_text = main_text
    module_inquiry = module_text
    module_override = module_text
    fixed_inquiry = fixed(text)
    if (len(main_inquiry) /= 5 .or. main_inquiry /= 'abcde') error stop
    if (len(module_inquiry) /= 5 .or. module_inquiry /= 'abcde') error stop
    if (len(module_override) /= 5 .or. module_override /= 'abcde') error stop
    if (len(fixed_inquiry) /= 5 .or. fixed_inquiry /= 'abcde') error stop
    if (len(size_inquiry) /= 5 .or. len(upper_inquiry) /= 5) error stop
    if (len(lower_inquiry) /= 1 .or. len(rank_inquiry) /= 1) error stop
    if (check_function(text) /= 'abcde') error stop
    if (external_half(text) /= 'abcde') error stop
    call check_subroutine(text)
    call check_internal()
    block
        character(*), parameter :: block_text = 'abcdeedcba'
        character(len(half(block_text))) :: t
        t = half(block_text)
        if (len(t) /= 5 .or. t /= 'abcde') error stop
    end block
contains
    subroutine check_internal()
        character(*), parameter :: internal_text = 'abcdeedcba'
        character(len(half(internal_text))) :: t
        character(generic_length(10)) :: generic_text
        t = half(internal_text)
        generic_text = t
        if (len(t) /= 5 .or. t /= 'abcde') error stop
        if (len(generic_text) /= 5 .or. generic_text /= t) error stop
    end subroutine
end program

function external_half(s) result(t)
    use spec_expr_11_mod, only: half
    character(*), intent(in) :: s
    character(len(half(s))) :: t
    t = half(s)
end function

pure integer function spec_expr_11_length(n)
    integer, intent(in) :: n
    spec_expr_11_length = n/2
end function

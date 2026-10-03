module template_array_08_m
    implicit none
    requirement binary_array {t, combine}
        deferred type :: t
        deferred interface
            function combine(x, y) result(r)
                type(t), intent(in) :: x(2), y(2)
                type(t) :: r(2)
            end function
        end interface
    end requirement

    template array_results {t, combine}
        require :: binary_array {t, combine}
        interface operator(+)
            procedure combine
        end interface
    contains
        subroutine from_locals(x, y, a)
            type(t), intent(in) :: x(2), y(2)
            type(t), intent(out) :: a(2)
            type(t) :: left(2), right(2)
            left = x
            right = y
            a = left + right
        end subroutine

        subroutine from_scalars(x, y, a)
            type(t), intent(in) :: x, y
            type(t), intent(out) :: a(2)
            a = [x, y]
        end subroutine
    end template
contains
    function select_chars(x, y) result(r)
        character, intent(in) :: x(2), y(2)
        character :: r(2)
        r(1) = x(1)
        r(2) = y(2)
    end function

    function select_words(x, y) result(r)
        character(4), intent(in) :: x(2), y(2)
        character(4) :: r(2)
        r(1) = x(1)
        r(2) = y(2)
    end function
end module

program template_array_08
    use template_array_08_m
    implicit none
    ! Experimental template syntax is not supported by GFortran.
    instantiate array_results {character, select_chars}, &
        only: locals_char => from_locals, scalars_char => from_scalars
    instantiate array_results {character(4), select_words}, &
        only: locals_word => from_locals, scalars_word => from_scalars

    character :: chars(2), x, y
    character(4) :: words(2)
    character, allocatable :: allocated(:)
    character, pointer :: view(:)
    character, target :: storage(2)

    call locals_char(["a", "b"], ["y", "z"], chars)
    if (any(chars /= ["a", "z"])) error stop
    x = "c"
    y = "d"
    call scalars_char(x, y, chars)
    if (any(chars /= ["c", "d"])) error stop
    call scalars_char("e", "f", chars)
    if (any(chars /= ["e", "f"])) error stop

    allocate(allocated(2))
    call locals_char(["g", "h"], ["i", "j"], allocated)
    if (any(allocated /= ["g", "j"])) error stop
    view => storage
    call scalars_char("k", "l", view)
    if (any(storage /= ["k", "l"])) error stop
    deallocate(allocated)

    call locals_word(["left", "skip"], ["skip", "last"], words)
    if (any(words /= ["left", "last"])) error stop
    call scalars_word("east", "west", words)
    if (any(words /= ["east", "west"])) error stop
end program

program select_type_48
    implicit none
    character(5) :: a(10)
    a = "hello"
    call sub(a)
contains
    subroutine sub(x)
        class(*), intent(in) :: x(:)
        if (size(x) /= 10) error stop 1
        select type (x)
        type is (character(*))
            if (x(1) /= "hello") error stop 2
        class default
            error stop 3
        end select
    end subroutine
end program

program select_type_37
    implicit none
    character(5) :: a(2)
    a(1) = 'hello'
    a(2) = 'world'
    call sub(a)
contains
    subroutine sub(x)
        class(*), intent(in) :: x(:)
        integer :: u, n
        character(100) :: buf
        select type (x)
        type is (character(*))
            ! Unformatted write of character array
            open(newunit=u, status='scratch', form='unformatted', access='stream')
            write(u) x
            close(u)

            ! Formatted write of character array
            write(buf, '(2A5)') x
            if (buf(1:5) /= 'hello') error stop
            if (buf(6:10) /= 'world') error stop
        class default
            error stop
        end select
        print *, "PASS"
    end subroutine
end program

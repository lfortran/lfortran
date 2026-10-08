program mre_select_type_char_len
    implicit none
    character(len=5) :: s
    s = "xxxxx"
    call g(s)
    print '(3a)', '[', s, ']'
    if (s /= "hi") error stop
contains
    subroutine g(value)
        class(*), intent(inout) :: value
        select type (value)
        type is (character(len=*))
            value = "hi"
            print *, len(value)
        end select
    end subroutine
end program
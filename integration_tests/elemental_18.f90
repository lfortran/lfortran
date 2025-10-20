program elemental_18
    integer :: arr(10)
    logical :: logi
    character(len=1) :: str_arr(10)

    str_arr = "a"
    arr = 97 ! ASCII equivalent of 'a'

    logi = all(peek(arr) == str_arr)
    if(logi .neqv. .true.) error stop

    contains 
    elemental function peek(pos) result(ch)
        integer, intent(in) :: pos
        character(1) :: ch
        ch = achar(pos)
    end function peek
end program

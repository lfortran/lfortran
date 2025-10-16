! Test automatic reallocation of allocatable array of strings (string length is fixed)
program functions_47

    call sub()
    contains

subroutine sub()
    character(10) :: string
    character(len=1), allocatable :: chars(:)
    chars = foo2([1, 1, 1]) ! <<<< Test automatic reallocation
    if(all(chars /= 'a'))   error stop
    if(len(chars(1:1)) /= 1) error stop
end subroutine 

elemental function foo2(pos) result(character_string)
        integer, intent(in) :: pos
        character(len=1) :: character_string
        character_string = 'a'
    end function 

end program

program functions_59
    character(10),allocatable :: words_arr(:,:)
    allocate(words_arr(20,30))
    call test(words_arr)

    contains 
    subroutine test(words)
        character(len=*), intent(in) :: words(:,:)
        character (len= (size(words)) + sum((shape(words)))) :: str
        print *, len(str)
        if(len(str)/= 650) error stop
    end subroutine test
end program

! Test calling `foo2` that has a string return of length depending on the input array size. 
program functions_46
    character(1) :: arr(12)
    call foo(arr)

    contains

    pure function foo2(set) result(string)
        character(len=1), intent(in) :: set(:)
        character(len=size(set)) :: string !!! <<< Testing this
    end function foo2



    subroutine foo(set)
        character(len=1), intent(in) :: set(:)
        character(len=:), allocatable :: chomped_string

        print *, len(foo2(set))
        if(len(foo2(set)) /= size(set)) error stop

    end subroutine foo

end program 
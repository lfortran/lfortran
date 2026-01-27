program test_string_concat
    implicit none

    type :: string_t
        character(5) :: s
    end type string_t

    type(string_t) :: t
    character(:), allocatable :: result

    t%s = "World"
    result = "Hello " // t%s
    if (result /= "Hello World") error stop "wrong string"
    if (len(result) /= 11)      error stop "wrong length"

    print *, "test passed"
end program test_string_concat

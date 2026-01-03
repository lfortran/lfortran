program string_68
    character(len=5) :: string = "hello"
    integer :: x = 1
    character(1) :: arr(3)

    print *, ["aa", string(x+1:x+2), "baa"]

    arr = ["aa", string(x+1:x+2), "baa"]
    print *,arr
    if(any(arr /= ["a", "e", "b"])) error stop
    
end program string_68
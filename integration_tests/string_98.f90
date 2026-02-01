program trim_param_concat
    implicit none
    character(len=*), parameter :: s = 'hello'
    character(len=*), parameter :: t = 'world'
    character(len=6) :: r1
    character(len=10) :: r2

    r1 = trim(s)//'x'
    if (r1 /= 'hellox') error stop

    r2 = trim(s)//trim(t)
    if (r2 /= 'helloworld') error stop

    print *, "PASSED"
end program

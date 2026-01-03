program case_07
    implicit none
    if (test_case('a') /= "a:j") error stop
    if (test_case('d') /= "a:j") error stop
    if (test_case('l') /= "l:p") error stop
    if (test_case('p') /= "l:p") error stop
    if (test_case('z') /= "z") error stop
    if (test_case('k') /= "not found") error stop
    if (test_case('r') /= "not found") error stop
contains
    function test_case(c) result(res)
        character(len=1), intent(in) :: c
        character(len=10)            :: res
        select case (c)
            case ('a' : 'j')
                res = "a:j"
                print *, 'one of the first ten letters'
            case ('l' : 'p')
                res = "l:p"
                print *, 'one of l, m, n, o, p'
            case ('z')
                res = "z"
                print *, 'one of z'
            case default
                res = "not found"
                print *, 'other characters, which may not be letters'
        end select
    end function test_case
end program case_07

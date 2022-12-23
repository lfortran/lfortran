program case_07
    character(len=1) :: c

    select case (c)
    case ('a' : 'j')
        print *, 'one of the first ten letters'
    case ('l' : 'p')
        print *, 'one of l, m, n, o, p'
    case ('z')
        print *, 'one of z'
    case default
        print *, 'other characters, which may not be letters'
    end select

end program case_07

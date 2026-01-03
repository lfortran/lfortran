program string_44
    character(len=:),allocatable :: color
    color = "red"
    select case (color)
    case('red')
        print *, "red"
    end select
end program
program main
    integer(8) :: i = 121
    integer(4) :: j = 121
    print *, kind(mod(j, i))
end program
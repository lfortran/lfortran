program string_45
    character(len=5), parameter :: color = "hello"

    integer(1) :: i = 1
    integer(2) :: j = 2
    integer(4) :: k = 3
    integer(8) :: l = 4

    print *, color(i:i)
    if (color(i:i) /= "h") error stop

    print *, color(j:j)
    if (color(j:j) /= "e") error stop

    print *, color(k:k)
    if (color(k:k) /= "l") error stop

    print *, color(l:l)
    if (color(l:l) /= "l") error stop

    print *, color(5:5)
 end program string_45
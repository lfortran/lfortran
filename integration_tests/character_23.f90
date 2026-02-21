program character_23
    implicit none
    character,parameter:: abc*(*) = 'abc'
    character,parameter:: x*(*) = 'hello'
    character*(*),parameter:: y = 'world'

    if (abc /= 'abc') error stop
    if (len(abc) /= 3) error stop
    if (x /= 'hello') error stop
    if (len(x) /= 5) error stop
    if (y /= 'world') error stop
    if (len(y) /= 5) error stop

    print *, abc
    print *, x
    print *, y
end program character_23

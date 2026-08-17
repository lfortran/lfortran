program main
    implicit none
    character(32) :: name
    common /sn/ name

    if (name /= 'HELLO') error stop
end program

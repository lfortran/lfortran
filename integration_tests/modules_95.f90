program modules_95
    use modules_95_m
    implicit none
    type(t) :: a
    call set_k1(a, 2.0)
    call touch(a)
    if (a%k1 /= 2.0) error stop 1
end program

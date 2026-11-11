program modules_94
    use modules_94_m2
    implicit none
    type(t) :: a
    call set_k1(a, 2.0)
    if (a%k1 /= 2.0) error stop 1
end program

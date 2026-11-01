program modules_89
    use modules_89_mod_b, only: s
    implicit none
    integer :: x(3)
    call s(x)
    print *, x
    if (any(x /= [1, 2, 3])) error stop
end program modules_89

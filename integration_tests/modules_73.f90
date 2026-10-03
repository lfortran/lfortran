program modules_73
    use modules_73_mod_b, only: s
    integer :: k1, k2
    call s(k1, k2)
    print *, k1, k2
    if (k1 /= 5) error stop
    if (k2 /= 3) error stop
end program modules_73

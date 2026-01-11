module enum_06_mod_lower
    use enum_06_mod_middle
end module

program pp
    use enum_06_mod_lower
    print *, red
    if(red /= 100) error stop
end program
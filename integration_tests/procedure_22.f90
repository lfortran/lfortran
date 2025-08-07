program procedure_22
    use procedure_22_mod_b
    type(deps_t) :: temp_dpt
    temp_dpt%str = "Hello     "
    if (func(temp_dpt) /= 5) error stop
end program
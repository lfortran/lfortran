program test_derived_types_97
    use derived_types_97_mod2
    implicit none
    
    type(point_t) :: p
    
    call create_point(p)
    
    if (p%x /= 10) error stop
    if (p%y /= 20) error stop
    
    print *, "OK"
end program test_derived_types_97

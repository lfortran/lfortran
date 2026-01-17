module derived_types_97_mod2
    use derived_types_97_mod1
    implicit none
    
contains
    
    subroutine create_point(p)
        type(point_t), intent(out) :: p
        ! This uses keyword arguments with a type that has a generic interface
        p = point_t(x = 10, y = 20)
    end subroutine create_point
    
end module derived_types_97_mod2

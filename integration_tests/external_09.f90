subroutine map_xi_to_c (cs, cv)
    implicit none
    real, dimension(2), intent(in) :: cs
    real, intent(out) :: cv
    real, external :: fold
    cv=fold(cs(1))
    return
end

function fold (x0)
    implicit none
    real :: fold
    real :: x0
    fold = x0
end

program external_09
    implicit none
    real :: cv = 12.91
    real, dimension(2) :: cs = [12.92, -22.0]
    call map_xi_to_c(cs, cv)
    print *, cv
    if ( abs(cv - 12.92) > 1e-8 ) error stop
end program external_09


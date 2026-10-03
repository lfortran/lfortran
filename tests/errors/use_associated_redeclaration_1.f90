module use_associated_redeclaration_1_mod
    implicit none
    integer, parameter :: dp_ua = kind(0.0d0)
end module

program use_associated_redeclaration_1
    use use_associated_redeclaration_1_mod
    implicit none
    integer, parameter :: dp_ua = kind(0.0)
    real(dp_ua) :: r
    r = 1.0
    print *, r
end program

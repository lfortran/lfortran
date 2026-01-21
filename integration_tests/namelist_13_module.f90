module config_module
    implicit none
    integer :: param1 = 10
    integer :: param2 = 20
    real :: tolerance = 1.0e-6
    namelist /config/ param1, param2, tolerance
end module config_module

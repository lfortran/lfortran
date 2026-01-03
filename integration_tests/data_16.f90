module data_16_module
    public :: pi
    data pi /3.14/
end module

program data_16
    use data_16_module
    print *, "pi: ", pi
    if (abs(pi - 3.14) > 1.0e-6) error stop
end program data_16

program namelist_01
    integer :: x, y
    namelist /my_nml/ x, y
    x = 1
    y = 2
    write(*, nml=my_nml)
end program
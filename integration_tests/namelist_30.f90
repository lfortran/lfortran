module mod_namelist_30
    implicit none
    integer :: x = 123
end module mod_namelist_30

program namelist_30
    use mod_namelist_30, y => x
    implicit none
    
    character(len=80) :: page(3)
    namelist /nl/ y

    write(page, nl)
    print *, page
end program namelist_30

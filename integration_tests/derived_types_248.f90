! Array components of two same-named derived types from different modules
! are deep copied with each module's own copy function, also when the
! modules are compiled separately.
program derived_types_248
    use derived_types_248_module1, only: test1
    use derived_types_248_module2, only: test2
    implicit none
    call test1()
    call test2()
end program

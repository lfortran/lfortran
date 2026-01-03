module module_format7 
    character(len=10) :: a = '(es15.8e2)'
end module
program format7
    use module_format7
    print *, a
    print a, 1.0 
end program

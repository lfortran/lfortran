MODULE declaration_02_mod1
   integer (kind=4), parameter :: m = 3
   integer (kind=4), parameter :: n = 1000
END MODULE declaration_02_mod1

MODULE declaration_02_mod2
   use declaration_02_mod1
   integer (kind=4), dimension(m) :: arr1
   integer (kind=4), dimension(n) :: arr2
END MODULE declaration_02_mod2

program declaration_02 
    use declaration_02_mod1
    use declaration_02_mod2
    if (size(arr1) /= 3) error stop
    if ((size(arr2) /= 1000)) error stop
end program
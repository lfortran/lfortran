MODULE declaration_02_mod1
   integer (kind=4), parameter :: m = 3
END MODULE declaration_02_mod1

MODULE declaration_02_mod2
   use declaration_02_mod1
   integer (kind=4), dimension(m) :: arr1, arr2
END MODULE declaration_02_mod2

program declaration_02 
    use declaration_02_mod1
    use declaration_02_mod2
    if (size(arr1) /= 3 .or. size(arr2) /= 3) error stop
end program
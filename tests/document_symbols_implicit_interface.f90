module document_symbols_implicit_interface_m
contains
    real function use_g(y)
        real :: y
        real, external :: g
        external :: s
        use_g = g(y)
        call s(y)
    end function
end module

program document_symbols_implicit_interface
    use document_symbols_implicit_interface_m
    real, external :: g
    print *, g(1.0), use_g(2.0)
end program

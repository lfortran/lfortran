module m_diagnostic
    implicit none

    type :: toml_diagnostic
        integer, allocatable :: label(:)
    end type toml_diagnostic

    type :: toml_parser
        type(toml_diagnostic), allocatable :: diagnostic(:)
    end type toml_parser
end module m_diagnostic

program test_diag
    use m_diagnostic
    implicit none
    type(toml_parser), allocatable :: pars, pars2
    allocate(pars)
    allocate(pars2)
    allocate(pars%diagnostic(1))
    pars%diagnostic(1) = toml_diagnostic([1,2,3])
    pars2 = pars
    pars2%diagnostic(1)%label = [4,5,6]
    if (any(pars%diagnostic(1)%label /= [1,2,3])) error stop
end program test_diag
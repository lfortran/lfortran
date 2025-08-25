module m_labels_derived_types_80
    implicit none

    type :: toml_label
        character(:), allocatable :: source
    end type toml_label

    type :: toml_diagnostic
        type(toml_label), allocatable :: label
    end type toml_diagnostic

end module m_labels_derived_types_80

program derived_types_80
    use m_labels_derived_types_80
    implicit none

    type(toml_diagnostic) :: diag
    type(toml_label) :: lbl

    integer, allocatable :: x

    lbl%source = "Something went wrong"
    diag%label = lbl

    print *, diag%label%source
end program derived_types_80

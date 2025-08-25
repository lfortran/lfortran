module m_labels_derived_types_79
    implicit none

    type :: toml_label
        character(:), allocatable :: source
    end type toml_label

    type :: toml_diagnostic
        type(toml_label), allocatable :: label(:)
    end type toml_diagnostic

end module m_labels_derived_types_79


module m_render_derived_types_79
    use m_labels_derived_types_79
    implicit none

contains

    function render_diagnostic(d) result(out)
        type(toml_diagnostic), intent(in) :: d
        character(:), allocatable :: out
        if (allocated(d%label)) then
            out = "Diagnostic: " // d%label(1)%source
        else
            out = "Empty diagnostic"
        end if
    end function render_diagnostic

end module m_render_derived_types_79


program derived_types_79
    use m_labels_derived_types_79
    use m_render_derived_types_79
    implicit none

    type(toml_diagnostic) :: diag
    type(toml_label) :: lbl(1)

    lbl(1)%source = "Something went wrong"
    diag%label = lbl

    print *, render_diagnostic(diag)
end program derived_types_79

program test_iso_bindings
    use iso_c_binding
    implicit none

    interface
        subroutine run_all_cfi_tests() bind(c, name="run_all_cfi_tests")
        end subroutine
    end interface

    call run_all_cfi_tests()
end program test_iso_bindings
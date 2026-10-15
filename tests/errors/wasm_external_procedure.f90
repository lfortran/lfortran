program wasm_external_procedure
    ! The external function zz is not defined in this file, so the WASM
    ! backend, which does not link with other objects, cannot call it.
    implicit none
    interface
        integer function zz(k)
            integer, intent(in) :: k
        end function zz
    end interface
    print *, zz(2)
end program wasm_external_procedure

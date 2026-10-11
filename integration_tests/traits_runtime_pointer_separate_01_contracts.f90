module traits_runtime_pointer_separate_01_contracts
    implicit none
    abstract interface :: IValue
        integer function value()
        end function
        subroutine read_into(result)
            integer, intent(out) :: result
        end subroutine
    end interface
end module

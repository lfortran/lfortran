module proc_type_procedure_17
contains
 subroutine cb()
   implicit none
end subroutine
end module

module cobylb_mod_procedure_17
contains
    subroutine cobylb(call_back)
        use proc_type_procedure_17
        procedure(cb) :: call_back
        integer, save :: x = 3
        call calcfc_internal()
    contains
        subroutine calcfc_internal()
            implicit none
            integer :: y(x)
            print *, 'x = ', x
            if ( x /= 3 ) error stop
        end subroutine calcfc_internal
    end subroutine cobylb
end module

program procedure_17
use cobylb_mod_procedure_17
use proc_type_procedure_17
call cobylb(cb)
end program

module eval_dependency
contains
    subroutine OBJCON()
    end subroutine OBJCON
end module eval_dependency

module procedure_decl_01_b
contains
subroutine evaluate(calcfc)
    use eval_dependency
    implicit none
    procedure(OBJCON) :: calcfc 
    call calcfc()
end subroutine evaluate
end module procedure_decl_01_b
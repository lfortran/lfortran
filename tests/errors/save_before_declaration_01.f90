! A name in a SAVE statement that no type declaration declares has no type
! when no implicit type applies: in a procedure, in a BLOCK construct and in
! an interface body inside a BLOCK construct.
subroutine save_before_declaration_01_sub()
    implicit none
    save x
end subroutine

subroutine save_before_declaration_01_block()
    implicit none
    block
        save y
    end block
end subroutine

subroutine save_before_declaration_01_interface()
    implicit none
    block
        interface
            subroutine save_before_declaration_01_g()
                implicit none
                save z
            end subroutine
        end interface
    end block
end subroutine

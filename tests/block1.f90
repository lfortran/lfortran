program block1
    implicit integer(a-z)
    integer :: B
    ! TODO: Fix this test case
    block
    !     use mod, only: example
    !     import, none
    !     !import, only: B
         B = 10
         print *, B
         if (B == 10) then
            print *, "pass"
         end if
    end block
end
! The variable B is implicitly declared in the scoping unit of the main program.
! The statement IMPORT, NONE makes B inaccessible in the BLOCK construct.
! If the IMPORT, NONE statement is replaced with the IMPORT statement in the
! comment, the program is conformant.

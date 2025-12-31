! Common block with CHARACTER - setter subroutine
subroutine set_name()
    implicit none
    character(32) :: srnamt
    common /srnamc/ srnamt
    srnamt = 'HELLO'
end subroutine

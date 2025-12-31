! Common block with CHARACTER - reader subroutine
subroutine print_name()
    implicit none
    character(32) :: srnamt
    common /srnamc/ srnamt
    if (srnamt /= 'HELLO') error stop
end subroutine

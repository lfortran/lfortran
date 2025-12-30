! Common block with CHARACTER - main program
program separate_compilation_28
    implicit none
    character(32) :: srnamt
    common /srnamc/ srnamt
    call set_name()
    call print_name()
end program

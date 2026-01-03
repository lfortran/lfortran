program write5
    implicit none
    character(10) :: adv_val = "no"
    character(10) :: first_name, last_name

    first_name = "first"
    last_name = "last"

    write (*, "(a)", advance=adv_val) "hi"
    write (*, "(a)") "bye"

    write (*, "(a)", advance="n"//"o") first_name//", "
    write (*, "(a)", advance="y"//"e"//"s") last_name

    write(*,'("Hello ")',advance='NO')
    write(*,'("world!")')
end program

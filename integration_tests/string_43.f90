
program string_43
    character(len=2) :: textblock(2)
    textblock = f()
    print*, "lf"//textblock

    contains
    function f() result(textblock)
        character(len=256), allocatable :: textblock(:)
        allocate(textblock(2))
        textblock(1) = "lf"
        textblock(2) = "gf"
        textblock=[character(len=len(textblock)+1) :: ' ',textblock]
        textblock=' '//textblock
    end function
end program
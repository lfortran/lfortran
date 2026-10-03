module implied_do_loops43_mod
    implicit none
contains
    function paragraph() result(res)
        character(len=10), allocatable :: res(:)
        allocate(res(1))
        res(1) = 'def'
    end function paragraph

    function myfunc(name) result(textblock)
        character(len=*), intent(in) :: name
        character(len=256), allocatable :: textblock(:)
        character(len=256), allocatable :: narrow(:)
        
        allocate(narrow(0))
        narrow = [character(len=256) :: narrow, 'abc' // paragraph()]
        
        textblock = narrow 
    end function myfunc
end module implied_do_loops43_mod

program implied_do_loops43
    use implied_do_loops43_mod
    implicit none
    character(len=256), allocatable :: textblock(:)
    textblock = myfunc('help')
    if (size(textblock) /= 1) error stop 1
    if (textblock(1) /= 'abcdef') error stop 2
end program

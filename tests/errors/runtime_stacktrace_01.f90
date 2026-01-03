program expr2
    implicit none
    print *, main()

contains

    function main()
        integer :: main
        main = 1
        print *, g()
    end function main

    subroutine f()
        stop 1
    end subroutine f

    function g()
        integer :: g
        g = 1
        call f()
    end function g
end program expr2

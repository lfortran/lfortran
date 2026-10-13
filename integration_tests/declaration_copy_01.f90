module declaration_copy_01_m
    implicit none
    interface
        module function count(n, a, btext) result(r)
            integer, intent(in) :: n, a(n)
            character(len=n), intent(in) :: btext
            integer :: r
        end function
    end interface
end module

submodule(declaration_copy_01_m) declaration_copy_01_s
    implicit none
contains
    module procedure count
        r = 10 + sum(a) + len(btext)
    end procedure
end submodule

program declaration_copy_01
    use declaration_copy_01_m
    implicit none
    if (count(3, [1, 2, 3], "abc") /= 19) error stop 1
    if (count(1, [7], "q") /= 18) error stop 2
end program

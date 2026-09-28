! Enumerator initializers that reference named constants through a module
! entity, in a module, a main program and a procedure.
module namespace_modules_34_k
    implicit none
    integer, parameter :: k = 5
    integer, parameter :: offsets(2) = [10, 20]
end module

module namespace_modules_34_m
    use, namespace :: l => namespace_modules_34_k
    implicit none
    enum, bind(c)
        enumerator :: e1 = l%k, e2
        enumerator :: e3 = l%offsets(2) + l%k
    end enum
contains
    integer function next_after(x) result(r)
        integer, intent(in) :: x
        enum, bind(c)
            enumerator :: f1 = l%k * 2, f2
        end enum
        r = x + f2 - f1
    end function
end module

program namespace_modules_34
    use namespace_modules_34_m
    use, namespace :: l => namespace_modules_34_k
    implicit none
    enum, bind(c)
        enumerator :: g1 = l%offsets(1), g2, g3 = l%k
    end enum
    print *, e1, e2, e3
    if (e1 /= 5 .or. e2 /= 6 .or. e3 /= 25) error stop
    print *, g1, g2, g3
    if (g1 /= 10 .or. g2 /= 11 .or. g3 /= 5) error stop
    if (next_after(3) /= 4) error stop
    call sub()
contains
    subroutine sub()
        enum, bind(c)
            enumerator :: h1, h2 = l%k + 1
        end enum
        print *, h1, h2
        if (h1 /= 0 .or. h2 /= 6) error stop
    end subroutine
end program

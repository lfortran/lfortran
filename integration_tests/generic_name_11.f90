module generic_name_11_mod
    implicit none
contains
    function ff(i) result(r)
        integer, intent(in) :: i
        integer :: r
        r = 10 * i
    end function ff

    function gg() result(r)
        integer :: r
        r = 7
    end function gg
end module generic_name_11_mod

module generic_name_11_mod2
    use generic_name_11_mod
    implicit none
    ! The generic interface has the same name as one of its use associated
    ! specific procedures.
    interface ff
        module procedure ff
        module procedure gg
    end interface
end module generic_name_11_mod2

program generic_name_11
    use generic_name_11_mod2
    implicit none
    integer :: x

    x = ff(3)
    print *, x
    if (x /= 30) error stop

    x = ff()
    print *, x
    if (x /= 7) error stop
end program generic_name_11

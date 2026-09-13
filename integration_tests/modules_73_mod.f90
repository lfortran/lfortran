module modules_73_mod_a
    integer, parameter :: n = 5
    integer :: m = 3
end module modules_73_mod_a

module modules_73_mod_b
    use modules_73_mod_a, only: n, m
contains
    subroutine s(k1, k2)
        integer, intent(out) :: k1, k2
        character(len=:), allocatable :: c, d
        allocate(character(len=n) :: c)
        allocate(character(len=m) :: d)
        k1 = len(c)
        k2 = len(d)
    end subroutine s
end module modules_73_mod_b

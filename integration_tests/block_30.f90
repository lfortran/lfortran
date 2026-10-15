module block_30_m
    implicit none
    integer :: n = 3
end module block_30_m

program block_30
    ! A USE statement in a BLOCK construct, with interface bodies in the BLOCK
    ! and in a nested BLOCK that import its use-associated n for the size of
    ! an array dummy and the length of a character dummy.
    implicit none
    integer :: x(3)
    x = [1, 2, 3]
    block
        use block_30_m, only: n
        interface
            integer function total(a)
                import :: n
                integer, intent(in) :: a(n)
            end function total
        end interface
        if (total(x) /= 6) error stop
        block
            interface
                subroutine fill(s, k)
                    import :: n
                    character(len=n), intent(out) :: s
                    integer, intent(in) :: k
                end subroutine fill
            end interface
            character(len=3) :: s
            call fill(s, 2)
            if (s /= "222") error stop
        end block
    end block
    print *, "ok"
end program block_30

integer function total(a)
    use block_30_m, only: n
    implicit none
    integer, intent(in) :: a(n)
    total = sum(a)
end function total

subroutine fill(s, k)
    use block_30_m, only: n
    implicit none
    character(len=n), intent(out) :: s
    integer, intent(in) :: k
    integer :: i
    do i = 1, n
        s(i:i) = char(ichar('0') + k)
    end do
end subroutine fill

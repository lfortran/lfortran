module block_34_m
    implicit none
contains
    subroutine declare(k)
        ! Procedure pointers whose abstract interfaces import a variable of
        ! a BLOCK construct of a module procedure, in a loop and in nested
        ! BLOCK constructs.
        integer, intent(in) :: k
        integer :: i
        do i = 1, 2
            block
                integer :: n
                abstract interface
                    subroutine sub(a)
                        import :: n
                        integer, intent(inout) :: a(n)
                    end subroutine sub
                    function fun(a) result(r)
                        import :: n
                        integer, intent(in) :: a(n)
                        integer :: r(n)
                    end function fun
                end interface
                procedure(sub), pointer :: ps
                procedure(fun), pointer :: pf
                n = k + i
                ps => null()
                pf => null()
                if (associated(ps) .or. associated(pf)) error stop
                block
                    integer :: m
                    abstract interface
                        subroutine chr(a, c)
                            import :: n, m
                            integer, intent(in) :: a(n, m)
                            character(len=m), intent(in) :: c
                        end subroutine chr
                    end interface
                    procedure(chr), pointer :: pc, qc
                    m = 2
                    pc => null()
                    qc => pc
                    if (associated(pc) .or. associated(qc)) error stop
                    if (n /= k + i) error stop
                end block
            end block
        end do
    end subroutine declare
end module block_34_m

program block_34
    use block_34_m, only: declare
    implicit none
    call declare(3)
    call internal(5)
    print *, "ok"
contains
    subroutine internal(k)
        ! The same in an internal procedure
        integer, intent(in) :: k
        integer :: i
        do i = 1, 2
            block
                integer :: n
                abstract interface
                    function fun(a) result(r)
                        import :: n
                        integer, intent(in) :: a(n)
                        integer :: r(n)
                    end function fun
                    subroutine chr(c)
                        import :: n
                        character(len=n), intent(in) :: c
                    end subroutine chr
                end interface
                procedure(fun), pointer :: pf, qf
                procedure(chr), pointer :: pc
                n = k * i
                pf => null()
                qf => pf
                pc => null()
                if (associated(pf) .or. associated(qf)) error stop
                if (associated(pc)) error stop
                block
                    integer :: m
                    abstract interface
                        subroutine sub(a)
                            import :: n, m
                            integer, intent(inout) :: a(n + m)
                        end subroutine sub
                    end interface
                    procedure(sub), pointer :: ps
                    m = i
                    ps => null()
                    if (associated(ps)) error stop
                    if (n + m /= k*i + i) error stop
                end block
            end block
        end do
        if (k /= 5) error stop
    end subroutine internal
end program block_34

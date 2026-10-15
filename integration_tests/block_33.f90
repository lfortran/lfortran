program block_33
    ! Procedure pointers whose abstract interfaces import a variable n for
    ! the bound of a dummy argument, the bound of the result and the length
    ! of a character dummy argument, in the main program and in a BLOCK
    ! construct of it. The pointers are only declared, nullified, copied
    ! and inquired: no procedure can be defined with characteristics that
    ! depend on a variable of a BLOCK.
    implicit none
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
        subroutine chr(c)
            import :: n
            character(len=n), intent(in) :: c
        end subroutine chr
    end interface
    procedure(sub), pointer :: ps
    procedure(fun), pointer :: pf
    procedure(chr), pointer :: pc
    n = 2
    ps => null()
    pf => null()
    pc => null()
    if (associated(ps) .or. associated(pf) .or. associated(pc)) error stop
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
            subroutine chr(c)
                import :: n
                character(len=n), intent(in) :: c
            end subroutine chr
        end interface
        procedure(sub), pointer :: ps, qs
        procedure(fun), pointer :: pf, qf
        procedure(chr), pointer :: pc, qc
        n = 3
        ps => null()
        pf => null()
        pc => null()
        qs => ps
        qf => pf
        qc => pc
        if (associated(ps) .or. associated(qs)) error stop
        if (associated(pf) .or. associated(qf)) error stop
        if (associated(pc) .or. associated(qc)) error stop
        if (n /= 3) error stop
    end block
    if (n /= 2) error stop
    print *, "ok"
end program block_33

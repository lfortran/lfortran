! A polymorphic intent(out) dummy is reset from its dynamic type when that
! type is local to the main program, to a BLOCK (two BLOCKs define types of
! the same name) or to a procedure, and nothing is done for a type whose
! reset has no effect. The BLOCKs are named because, for a type of an
! unnamed BLOCK, GFortran emits symbols such as __vtab_block@1_Inblock, which
! the GNU assembler rejects.
module intent_out_dynamic_type_02_m
    implicit none
    type, abstract :: Base
        integer :: n = 7
    end type
    type :: Bare
        integer :: k
    end type
contains
    subroutine reset(x)
        class(Base), intent(out) :: x
        if (x%n /= 7) error stop 90
    end subroutine

    subroutine reset_bare(x)
        class(Bare), intent(out) :: x
        x%k = 5
    end subroutine
end module

program intent_out_dynamic_type_02
    use intent_out_dynamic_type_02_m
    implicit none
    type, extends(Base) :: Local
        integer :: extra = 11
    end type
    type(Local) :: a
    type(Bare) :: b
    a%n = 1
    a%extra = 2
    call reset(a)
    if (a%n /= 7 .or. a%extra /= 11) error stop 1
    b%k = 1
    call reset_bare(b)
    if (b%k /= 5) error stop 2
    first: block
        type, extends(Base) :: InBlock
            integer :: extra = 13
        end type
        type(InBlock) :: c
        c%n = 1
        c%extra = 2
        call reset(c)
        if (c%n /= 7 .or. c%extra /= 13) error stop 3
    end block first
    second: block
        type, extends(Base) :: InBlock
            integer :: extra = 19
        end type
        type(InBlock) :: c
        c%n = 1
        c%extra = 2
        call reset(c)
        if (c%n /= 7 .or. c%extra /= 19) error stop 4
    end block second
    call inner()
    print '(a)', 'intent_out_dynamic_type_02 ok'
contains
    subroutine inner()
        type, extends(Base) :: InProcedure
            integer :: extra = 17
        end type
        type(InProcedure) :: d
        d%n = 1
        d%extra = 2
        call reset(d)
        if (d%n /= 7 .or. d%extra /= 17) error stop 5
    end subroutine
end program

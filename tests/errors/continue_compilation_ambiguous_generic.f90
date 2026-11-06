! The specific procedures of a generic interface shall be distinguishable
! (F2018 C1514). Each pair that is not is reported.
module continue_compilation_ambiguous_generic
    implicit none
    type :: amb_base
        integer :: i
    end type
    type, extends(amb_base) :: amb_child
    end type

    interface amb_g1
        module procedure amb_sa, amb_sb  ! {Error} ambiguous interfaces in generic interface 'amb_g1' for 'amb_sa' and 'amb_sb'
    end interface

    interface amb_g2
        module procedure amb_ta
        module procedure amb_tb  ! {Error} ambiguous interfaces in generic interface 'amb_g2' for 'amb_ta' and 'amb_tb'
    end interface

    interface amb_g3
        module procedure amb_ua, amb_ub  ! {Error} ambiguous interfaces in generic interface 'amb_g3' for 'amb_ua' and 'amb_ub'
    end interface

    interface amb_g4
        module procedure amb_va, amb_vb  ! {Error} ambiguous interfaces in generic interface 'amb_g4' for 'amb_va' and 'amb_vb'
    end interface

    interface amb_g5
        module procedure amb_wa, amb_wb  ! {Error} ambiguous interfaces in generic interface 'amb_g5' for 'amb_wa' and 'amb_wb'
    end interface

    ! Distinguishable by type: no error.
    interface amb_ok
        module procedure amb_sa, amb_ua
    end interface

    interface amb_g6
        subroutine amb_ext_a(x)
            real(8), intent(in) :: x
        end subroutine
        subroutine amb_ext_b(y)  ! {Error} ambiguous interfaces in generic interface 'amb_g6' for 'amb_ext_a' and 'amb_ext_b'
            real(8), intent(in) :: y
        end subroutine
    end interface

    ! CLASS(*) and TYPE(*) are type compatible with every entity.
    interface amb_g7
        module procedure amb_xa, amb_xb  ! {Error} ambiguous interfaces in generic interface 'amb_g7' for 'amb_xa' and 'amb_xb'
    end interface

    interface amb_g8
        module procedure amb_ya, amb_yb  ! {Error} ambiguous interfaces in generic interface 'amb_g8' for 'amb_ya' and 'amb_yb'
    end interface

    ! An assumed-rank dummy argument is rank compatible with every rank.
    interface amb_g9
        module procedure amb_za, amb_zb  ! {Error} ambiguous interfaces in generic interface 'amb_g9' for 'amb_za' and 'amb_zb'
    end interface

    interface amb_g10
        module procedure amb_qa, amb_qb  ! {Error} ambiguous interfaces in generic interface 'amb_g10' for 'amb_qa' and 'amb_qb'
    end interface

    ! A substring reference is not a procedure reference: amb_ra's dummy
    ! argument is a data object.
    interface amb_g11
        module procedure amb_ra, amb_rb  ! {Error} ambiguous interfaces in generic interface 'amb_g11' for 'amb_ra' and 'amb_rb'
    end interface

    ! TKR compatible by F2018, but left undecided: an assumed-rank dummy
    ! argument against one of another rank, either unlimited polymorphic or
    ! of assumed type (gfortran accepts these). No error.
    interface amb_undecided
        module procedure amb_qa, amb_pb
    end interface
contains
    subroutine amb_sa(x)
        real(8), intent(in) :: x
    end subroutine

    subroutine amb_sb(y)
        real(8), intent(in) :: y
    end subroutine

    subroutine amb_ta(n, x)
        integer, intent(in) :: n
        real, intent(in) :: x
    end subroutine

    subroutine amb_tb(k, y)
        integer, intent(in) :: k
        real, intent(in) :: y
    end subroutine

    subroutine amb_ua(n)
        integer, intent(in) :: n
    end subroutine

    subroutine amb_ub(n, x)
        integer, intent(in) :: n
        real, intent(in), optional :: x
    end subroutine

    ! A call with keywords, amb_g4(x=1.0, n=2), matches both.
    subroutine amb_va(x, n)
        real, intent(in) :: x
        integer, intent(in) :: n
    end subroutine

    subroutine amb_vb(n, x)
        integer, intent(in) :: n
        real, intent(in) :: x
    end subroutine

    subroutine amb_wa(a)
        class(amb_base), intent(in) :: a
    end subroutine

    subroutine amb_wb(b)
        type(amb_child), intent(in) :: b
    end subroutine

    subroutine amb_xa(x)
        class(*), intent(in) :: x
    end subroutine

    subroutine amb_xb(x)
        integer, intent(in) :: x
    end subroutine

    subroutine amb_ya(x)
        type(*), intent(in) :: x
    end subroutine

    subroutine amb_yb(x)
        real, intent(in) :: x
    end subroutine

    subroutine amb_za(x)
        integer, intent(in) :: x(..)
    end subroutine

    subroutine amb_zb(x)
        integer, intent(in) :: x(:)
    end subroutine

    subroutine amb_qa(x)
        class(*), intent(in) :: x(..)
    end subroutine

    subroutine amb_qb(x)
        integer, intent(in) :: x(..)
    end subroutine

    subroutine amb_pb(x)
        integer, intent(in) :: x(:,:)
    end subroutine

    subroutine amb_ra(s)
        character(len=*) :: s
        print *, s(1:1)
    end subroutine

    subroutine amb_rb(t)
        character(len=*) :: t
        print *, t
    end subroutine
end module

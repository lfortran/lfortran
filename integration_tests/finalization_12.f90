! The local variables of a BLOCK construct are finalized when the construct
! completes, also when it is left by EXIT, CYCLE or RETURN (F2018 7.5.6.3
! p3); so is the result of a function that is the selector of an ASSOCIATE
! construct (p5).
module finalization_12_m
    implicit none
    integer :: nfin = 0
    integer :: fin_log(10) = 0
    type :: t
        integer :: v = 0
        integer, allocatable :: a(:)
    contains
        final :: finish
    end type
contains
    subroutine finish(x)
        type(t), intent(inout) :: x
        nfin = nfin + 1
        if (nfin <= size(fin_log)) fin_log(nfin) = x%v
    end subroutine

    function make(v) result(r)
        integer, intent(in) :: v
        type(t) :: r
        r%v = v
        allocate(r%a(v))
    end function

    subroutine reset()
        nfin = 0
        fin_log = 0
    end subroutine

    subroutine block_cycle()
        integer :: i
        do i = 1, 3
            block
                type(t) :: a
                real :: w(i)
                a%v = i
                w = 1
                if (i <= 3) cycle
                error stop "block_cycle"
            end block
        end do
    end subroutine

    subroutine block_return(n)
        integer, intent(in) :: n
        integer :: i
        do i = 1, 5
            block
                type(t) :: a
                a%v = 10 + i
                block
                    type(t) :: b
                    b%v = 20 + i
                    if (i == n) return
                end block
            end block
        end do
        error stop "block_return"
    end subroutine

    subroutine associate_exits()
        integer :: i
        outer: do i = 1, 3
            associate (x => make(i))
                if (i == 1) cycle outer
                if (i == 2) cycle
                if (x%v == 3) exit outer
            end associate
        end do outer
    end subroutine

    subroutine associate_return()
        associate (x => make(7))
            if (x%v == 7) return
        end associate
        error stop "associate_return"
    end subroutine
end module

program finalization_12
    use finalization_12_m
    implicit none

    call reset()
    call block_cycle()
    if (nfin /= 3) error stop 1
    if (any(fin_log(1:3) /= [1, 2, 3])) error stop 2

    call reset()
    call block_return(2)
    if (nfin /= 4) error stop 3
    if (any(fin_log(1:4) /= [21, 11, 22, 12])) error stop 4

    call reset()
    call associate_exits()
    if (nfin /= 3) error stop 5
    if (any(fin_log(1:3) /= [1, 2, 3])) error stop 6

    call reset()
    call associate_return()
    if (nfin /= 1) error stop 7
    if (fin_log(1) /= 7) error stop 8
    print *, "ok"
end program

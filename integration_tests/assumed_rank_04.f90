program assumed_rank_04
    implicit none
    call sub()
    print *, "ok"
contains
    subroutine sub(a1)
        class(*), optional, intent(in), dimension(..) :: a1
        if (present(a1)) error stop
    end subroutine sub
end program assumed_rank_04

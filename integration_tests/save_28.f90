! A SAVE statement may name a variable before its type declaration
module save_28_mod
    implicit none
contains
    recursive subroutine save_28_count(depth, total)
        integer, intent(in) :: depth
        integer, intent(out) :: total
        save n
        integer :: n
        if (depth == 0) n = 0
        n = n + 1
        if (depth < 2) call save_28_count(depth + 1, total)
        total = n
    end subroutine

    recursive subroutine save_28_count_block(depth, total)
        integer, intent(in) :: depth
        integer, intent(out) :: total
        block
            save n
            integer :: n
            if (depth == 0) n = 0
            n = n + 1
            if (depth < 2) call save_28_count_block(depth + 1, total)
            total = n
        end block
    end subroutine
end module

program save_28
    use save_28_mod, only: save_28_count, save_28_count_block
    implicit none
    integer :: total
    call save_28_count(0, total)
    if (total /= 3) error stop
    call save_28_count_block(0, total)
    if (total /= 3) error stop
    print *, total
end program

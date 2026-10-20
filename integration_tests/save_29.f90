! A SAVE statement may name a variable before its type declaration
module save_29_mod
    implicit none
contains
    recursive subroutine save_29_count(depth, total)
        integer, intent(in) :: depth
        integer, intent(out) :: total
        save n
        integer :: n
        if (depth == 0) n = 0
        n = n + 1
        if (depth < 2) call save_29_count(depth + 1, total)
        total = n
    end subroutine

    recursive subroutine save_29_count_block(depth, total)
        integer, intent(in) :: depth
        integer, intent(out) :: total
        block
            save n
            integer :: n
            if (depth == 0) n = 0
            n = n + 1
            if (depth < 2) call save_29_count_block(depth + 1, total)
            total = n
        end block
    end subroutine
end module

program save_29
    use save_29_mod, only: save_29_count, save_29_count_block
    implicit none
    integer :: total
    call save_29_count(0, total)
    if (total /= 3) error stop
    call save_29_count_block(0, total)
    if (total /= 3) error stop
    print *, total
end program

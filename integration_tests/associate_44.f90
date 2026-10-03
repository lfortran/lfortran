program associate_44
    implicit none
    integer :: n
    n = 3
    associate(m => n)
        block
            logical :: p(m)
            p = .true.
            if (.not. all(p)) error stop
            if (size(p) /= 3) error stop
        end block
    end associate

    n = 5
    associate(m => n)
        block
            integer :: a(m)
            a = 7
            if (a(1) /= 7) error stop
            if (a(5) /= 7) error stop
            if (sum(a) /= 35) error stop
        end block
    end associate
    print *, "ok"
end program

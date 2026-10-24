program do_concurrent_24
    implicit none
    integer :: i, j, result(3), nested(3, 2)

    i = 100
    j = 200
    do concurrent (i = 1:3)
        block
            integer :: a(i)
            a = 10
            result(i) = size(a)
        end block
    end do
    if (any(result /= [1, 2, 3])) error stop

    do concurrent (i = 1:3)
        block
            integer :: i__dc, i__dc1, j__dc
            character(len=i) :: text
            i__dc = 10
            i__dc1 = 20
            j__dc = 30
            text = 'abc'
            do concurrent (j = 1:2)
                block
                    integer :: a(i, j)
                    character(len=i+j) :: words(j)
                    a = i__dc + i__dc1 + j__dc
                    words = 'abcde'
                    nested(i, j) = size(a) + len(text) + len(words) + size(words)
                    if (any(a /= 60)) error stop
                end block
            end do
        end block
    end do
    if (any(nested(:, 1) /= [5, 8, 11])) error stop
    if (any(nested(:, 2) /= [8, 12, 16])) error stop
    if (i /= 100 .or. j /= 200) error stop
end program

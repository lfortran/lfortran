module select_case_04_m
    implicit none
contains
    integer function classify(i, j, k) result(r)
        integer, intent(in) :: i, j, k
        r = 0
        select case (i)
        case (1)
            select case (j)
            case (2)
                select case (k)
                case (3)
                    r = 123
                case default
                    r = 120
                end select
            case default
                r = 100
            end select
        case default
            r = -1
        end select
    end function

    integer function deep(i, j, k, n) result(r)
        integer, intent(in) :: i, j, k, n
        integer :: m
        r = 0
        select case (i)
        case (1:5)
            select case (j)
            case default
                do m = 1, n
                    if (m > 0) then
                        select case (k)
                        case (:0)
                            r = r - 1
                        case (1, 2)
                            select case (m)
                            case (2)
                                r = r + 100
                            case default
                                r = r + 1
                            end select
                        case default
                            r = r + 10
                        end select
                    end if
                end do
            end select
        case default
            r = -1
        end select
    end function

    integer function in_while(i, n) result(r)
        integer, intent(in) :: i, n
        integer :: m
        r = 0
        m = 0
        select case (i)
        case (1)
            do while (m < n)
                m = m + 1
                select case (mod(m, 2))
                case (0)
                    select case (m)
                    case (4:)
                        r = r + 1000
                    case default
                        r = r + 10
                    end select
                case default
                    r = r + 1
                end select
            end do
        end select
    end function

    function letters(a, b, c) result(r)
        character(len=*), intent(in) :: a, b, c
        character(len=3) :: r
        r = '???'
        select case (a)
        case ('x')
            select case (b // 'z')
            case ('yz')
                select case (c(1:1))
                case ('a':'m')
                    r = 'xyl'
                case default
                    r = 'xyh'
                end select
            end select
        end select
    end function

    integer function parse(w1, w2, w3) result(r)
        character(len=*), intent(in) :: w1, w2, w3
        integer :: n
        r = 0
        select case (w1)
        case ('state')
            do n = 1, 2
                select case (w2)
                case ('geometry')
                    select case (w3)
                    case ('rectangle')
                        r = r + 1
                    case ('circle')
                        r = r + 2
                    case default
                        r = r + 3
                    end select
                case ('density')
                    r = r + 10
                end select
            end do
        case default
            r = -1
        end select
    end function
end module

program select_case_04
    use select_case_04_m
    implicit none
    if (classify(1, 2, 3) /= 123) error stop
    if (classify(1, 2, 0) /= 120) error stop
    if (classify(1, 0, 0) /= 100) error stop
    if (classify(0, 0, 0) /= -1) error stop
    if (deep(1, 0, 1, 3) /= 102) error stop
    if (deep(1, 0, 0, 3) /= -3) error stop
    if (deep(1, 0, 5, 2) /= 20) error stop
    if (deep(9, 0, 0, 3) /= -1) error stop
    if (in_while(1, 5) /= 1013) error stop
    if (in_while(2, 5) /= 0) error stop
    if (letters('x', 'y', 'abc') /= 'xyl') error stop
    if (letters('x', 'y', 'z') /= 'xyh') error stop
    if (letters('x', 'q', 'a') /= '???') error stop
    if (letters('w', 'y', 'a') /= '???') error stop
    if (parse('state', 'geometry', 'rectangle') /= 2) error stop
    if (parse('state', 'geometry', 'circle') /= 4) error stop
    if (parse('state', 'geometry', 'point') /= 6) error stop
    if (parse('state', 'density', 'x') /= 20) error stop
    if (parse('end', 'geometry', 'circle') /= -1) error stop
    print '(a)', "OK"
end program

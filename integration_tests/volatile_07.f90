program volatile_07
    implicit none
    integer :: i

    do i = 1, 3
        block
            integer, save :: n = 0
            volatile :: n
            volatile :: m, a
            integer :: m = 0
            integer :: a(2) = [2, 4]
            integer, volatile :: inline_n = 0
            integer, save :: async_n = 0
            asynchronous :: async_n
            asynchronous :: async_a
            integer :: async_a(2) = [3, 5]
            integer :: automatic
            volatile :: automatic

            n = n + 1
            m = m + 1
            inline_n = inline_n + 1
            a(1) = a(1) + 1
            a(2) = a(2) + 2
            async_n = async_n + 1
            async_a(1) = async_a(1) + 1
            async_a(2) = async_a(2) + 2
            automatic = i
            automatic = automatic + 10
            if (n /= i .or. m /= i .or. inline_n /= i) error stop
            if (any(a /= [2 + i, 4 + 2*i])) error stop
            if (async_n /= i) error stop
            if (any(async_a /= [3 + i, 5 + 2*i])) error stop
            if (automatic /= i + 10) error stop

            block
                volatile :: async_n
                asynchronous :: m
                if (async_n /= i .or. m /= i) error stop
            end block
        end block
    end do
end program volatile_07

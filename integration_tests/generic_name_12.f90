module generic_name_12_mod
    implicit none

    ! `push` is a generic interface and at the same time one of its own
    ! specific procedures, so the specific one is stored under a mangled name.
    ! The other generic interfaces of this module must still resolve it to the
    ! function, no matter whether their own name sorts before or after `push`.
    interface push
        module procedure push, push_scaled
    end interface push

    interface add
        module procedure push, push_scaled
    end interface add

    interface store
        module procedure push, push_scaled
    end interface store

contains

    function push() result(r)
        integer :: r
        r = 1
    end function push

    function push_scaled(i) result(r)
        integer, intent(in) :: i
        integer :: r
        r = 10 * i
    end function push_scaled

end module generic_name_12_mod

program generic_name_12
    use generic_name_12_mod
    implicit none
    print *, push(), push(3)
    print *, add(), add(3)
    print *, store(), store(3)
    if (push() /= 1) error stop
    if (push(3) /= 30) error stop
    if (add() /= 1) error stop
    if (add(3) /= 30) error stop
    if (store() /= 1) error stop
    if (store(3) /= 30) error stop
end program generic_name_12

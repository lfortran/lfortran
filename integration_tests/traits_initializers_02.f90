program traits_initializers_02
    use traits_initializers_02_facade_m, only: Item => Renamed
    implicit none
    type(Item) :: x, y, z
    x = Item(value=5)
    y = Item(n=7)
    z = Item()
    if (x%n /= 15) error stop 1
    if (y%n /= 7) error stop 2
    if (z%n /= 0) error stop 3
    x = Item(6)
    if (x%n /= 16) error stop 4
end program

program traits_runtime_component_04
    use traits_runtime_component_04_facade_m, only: Item => Box, read_box
    implicit none
    type(Item) :: x, y
    x = Item(17)
    y = x
    deallocate(x%item)
    if (read_box(y) /= 17) error stop 1
    if (y%item%value() /= 17) error stop 2
    deallocate(y%item)
    if (allocated(y%item)) error stop 3
end program

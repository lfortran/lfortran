module mod_char_array_indexing_02
    implicit none
    character, parameter :: chars(0:9) = ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j"]
end module mod_char_array_indexing_02

program char_array_indexing_02
    use mod_char_array_indexing_02, only : chars
    implicit none
    character(len=1) :: x
    x = chars(3)
    print *, "x: ", x
    if (x /= "d") error stop
end program

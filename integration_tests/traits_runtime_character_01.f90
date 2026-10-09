program traits_runtime_character_01
    use traits_runtime_character_01_m
    implicit none
    type(Leaf) :: item
    type(Forwarder) :: worker
    call exercise(item, worker, item)
end program

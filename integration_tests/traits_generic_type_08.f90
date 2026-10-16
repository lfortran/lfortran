! Instances of the generic types of traits_generic_type_08_lib, compiled
! against its module file: `Box{real}`'s method constructs the unrelated
! ordinary type the library imports as `OtherBox`, and `Tag{real}`'s
! initializer and method construct the instance.
program traits_generic_type_08
    use traits_generic_type_08_lib
    implicit none
    type(Box{real}) :: b
    type(Tag{real}) :: t

    b = Box{real}(1.5, 4)
    if (b%probe() /= 40) error stop 1

    t = Tag{real}(1.5)
    if (t%get() /= 3) error stop 2
    t = t%reset()
    if (t%get() /= 5) error stop 3

    print *, b%probe(), t%get()
end program traits_generic_type_08

program test_coarrays_41
    implicit none

    type :: Inner
        integer(1) :: a
        integer(8) :: b
    end type Inner

    type :: Outer
        type(Inner) :: in_s
        integer(4)  :: c
    end type Outer

    type(Outer) :: obj

    ! 1. Assign values on image 1
    if (this_image() == 1) then
        obj%in_s%a = 42_1
        obj%in_s%b = 100_8
        obj%c      = 999_4
    else
        obj%in_s%a = 0_1
        obj%in_s%b = 0_8
        obj%c      = 0_4
    end if

    ! 2. Broadcast obj from image 1 to all images
    call co_broadcast(obj, source_image=1)

    ! 3. Assert values on all images
    if (obj%in_s%a /= 42_1) error stop
    if (obj%in_s%b /= 100_8) error stop
    if (obj%c /= 999_4) error stop

end program test_coarrays_41
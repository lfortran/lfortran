program test_size
    use iso_c_binding, only: c_size_t, c_sizeof
    implicit none

    type :: Inner
        integer(1) :: a
        integer(8) :: b    ! Potential 7 bytes of padding before b
    end type Inner

    type :: Outer
        type(Inner) :: in_s
        integer(4)  :: c
    end type Outer

    type(Outer) :: obj

    ! 1. Assign unique values on image 1
    if (this_image() == 1) then
        obj%in_s%a = 42_1
        obj%in_s%b = 100_8
        obj%c      = 999_4
    end if

    ! 2. Broadcast obj from image 1 to all other active images
    call co_broadcast(obj, source_image=1)

    ! 3. Print the verified byte size on every image
    print *, "Image:", this_image(), "Actual LLVM Byte Size:", c_sizeof(obj)
end program test_size

module scoped_attribute_01_original
    implicit none
    real :: a(4), v(4)
end module

module scoped_attribute_01_wrapper
    use scoped_attribute_01_original, only: renamed => a, v
    implicit none
    asynchronous :: renamed
    volatile :: v
end module

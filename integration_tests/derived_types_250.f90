! Array components of same-named derived types declared in same-named module
! procedures, and in BLOCKs, of two modules are deep copied with each
! module's own copy function when the modules are compiled separately.
program derived_types_250
    use derived_types_250_module1, only: work1 => work, block_work1 => block_work
    use derived_types_250_module2, only: work2 => work, block_work2 => block_work
    implicit none
    call work1()
    call work2()
    call block_work1()
    call block_work2()
end program

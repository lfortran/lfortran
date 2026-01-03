program intrinsics_37
real(8) :: tsource(2, 3), fsource(2, 3), ar1(2, 3), ar2(2, 3)
logical :: mask(2, 3)
real(8) :: real_mask(2, 3)
integer :: i, j

tsource(1, 1) = 1
tsource(1, 2) = 4
tsource(1, 3) = 2
tsource(2, 1) = 5
tsource(2, 2) = 3
tsource(2, 3) = 6

fsource(1, 1) = 7
fsource(1, 2) = 0
fsource(1, 3) = 8
fsource(2, 1) = -1
fsource(2, 2) = 9
fsource(2, 3) = -2

mask(1, 1) = .true.
mask(1, 2) = .false.
mask(1, 3) = .false.
mask(2, 1) = .true.
mask(2, 2) = .true.
mask(2, 3) = .false.

do i = 1, ubound(mask, 1)
    do j = 1, ubound(mask, 2)
        if( mask(i, j) ) then
            real_mask(i, j) = 1.0_8
        else
            real_mask(i, j) = 0.0_8
        end if
    end do
end do

ar1 = merge(tsource, fsource, mask)
ar2 = tsource * real_mask + fsource * (1.0_4 - real_mask)

print *, ar1 - ar2

end program
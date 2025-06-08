program data_15
implicit none

real ABC(2,3,4,5)
real :: sum_ABC
real, parameter :: expected_sum_ABC = 345.6

! Initialize 4D array ABC (2x3x4x5 = 120 elements)
data ABC/ &
    1.11, 1.12, 1.13, 1.14, 1.15, 1.21, 1.22, 1.23, 1.24, 1.25, 1.31, 1.32, 1.33, 1.34, 1.35, &
    1.41, 1.42, 1.43, 1.44, 1.45, 1.51, 1.52, 1.53, 1.54, 1.55, 1.61, 1.62, 1.63, 1.64, 1.65, &
    2.11, 2.12, 2.13, 2.14, 2.15, 2.21, 2.22, 2.23, 2.24, 2.25, 2.31, 2.32, 2.33, 2.34, 2.35, &
    2.41, 2.42, 2.43, 2.44, 2.45, 2.51, 2.52, 2.53, 2.54, 2.55, 2.61, 2.62, 2.63, 2.64, 2.65, &
    3.11, 3.12, 3.13, 3.14, 3.15, 3.21, 3.22, 3.23, 3.24, 3.25, 3.31, 3.32, 3.33, 3.34, 3.35, &
    3.41, 3.42, 3.43, 3.44, 3.45, 3.51, 3.52, 3.53, 3.54, 3.55, 3.61, 3.62, 3.63, 3.64, 3.65, &
    4.11, 4.12, 4.13, 4.14, 4.15, 4.21, 4.22, 4.23, 4.24, 4.25, 4.31, 4.32, 4.33, 4.34, 4.35, &
    4.41, 4.42, 4.43, 4.44, 4.45, 4.51, 4.52, 4.53, 4.54, 4.55, 4.61, 4.62, 4.63, 4.64, 4.65 /

print *, "4D array ABC elements:"
print *, "ABC(1,1,1,1) =", ABC(1,1,1,1)
print *, "ABC(2,3,4,5) =", ABC(2,3,4,5)
print *, "ABC(1,2,3,4) =", ABC(1,2,3,4)
print *, "ABC(2,1,3,2) =", ABC(2,1,3,2)
print *, "ABC(1,3,2,5) =", ABC(1,3,2,5)
print *, "ABC(2,2,2,2) =", ABC(2,2,2,2)
print *, "ABC(1,3,4,1) =", ABC(1,3,4,1)
print *, "ABC(2,3,1,5) =", ABC(2,3,1,5)

! Verify ABC values
if (abs(ABC(1,1,1,1) - 1.11) > 1.0e-5) error stop "ABC(1,1,1,1) value is incorrect"
if (abs(ABC(2,3,4,5) - 4.65) > 1.0e-5) error stop "ABC(2,3,4,5) value is incorrect"
if (abs(ABC(1,2,3,4) - 3.62) > 1.0e-5) error stop "ABC(1,2,3,4) value is incorrect"
if (abs(ABC(2,1,3,2) - 2.23) > 1.0e-5) error stop "ABC(2,1,3,2) value is incorrect"
if (abs(ABC(1,3,2,5) - 4.42) > 1.0e-5) error stop "ABC(1,3,2,5) value is incorrect"
if (abs(ABC(2,2,2,2) - 2.14) > 1.0e-5) error stop "ABC(2,2,2,2) value is incorrect"
if (abs(ABC(1,3,4,1) - 1.53) > 1.0e-5) error stop "ABC(1,3,4,1) value is incorrect"
if (abs(ABC(2,3,1,5) - 4.32) > 1.0e-5) error stop "ABC(2,3,1,5) value is incorrect"

! Check last few elements
print *, "Last few ABC elements:"
print *, "ABC(2,3,4,3) =", ABC(2,3,4,3)
print *, "ABC(2,3,4,4) =", ABC(2,3,4,4)
print *, "ABC(2,3,4,5) =", ABC(2,3,4,5)
if (abs(ABC(2,3,4,3) - 3.32) > 1.0e-5) error stop "ABC(2,3,4,3) value is incorrect"
if (abs(ABC(2,3,4,4) - 4.21) > 1.0e-5) error stop "ABC(2,3,4,4) value is incorrect"
if (abs(ABC(2,3,4,5) - 4.65) > 1.0e-5) error stop "ABC(2,3,4,5) value is incorrect"

print *, "Calculating sum..."

! Use intrinsic sum function to calculate the sums
sum_ABC = sum(ABC)

print *, "Sum of all ABC elements:", sum_ABC

! Verify sums
if (abs(sum_ABC - expected_sum_ABC) > 1.0e-5) then
    print *, "Expected ABC sum:", expected_sum_ABC, "Actual:", sum_ABC
    error stop "Sum of ABC array is incorrect"
end if

print *, "Ok!"


end program

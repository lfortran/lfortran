program intrinsics_240
character(len=100) :: line1 = "     Hello, World!"
character(len=7) :: line2 = "Okay   "
character(5) :: line3 = " #o$ "
character(1) :: line4 = "  "
integer, parameter :: l1 = len_trim("     Hello, World!")
integer, parameter :: l2 = len_trim("Okay   ")
integer, parameter :: l3 = len_trim(" #o$ ")
integer, parameter :: l4 = len_trim("  ")

character(len = 3) :: arr1(3) = [" bc", "d f", "gh "]
character :: arr2(3) = ["  ", "y ", "o "]
integer, parameter :: l5(3) = len_trim([" bc", "d f", "gh "])
integer, parameter :: l6(4) = len_trim(["  ", "ok", "y ", "o "])

print*, l1
if (l1 /= 18 ) error stop
print*, l2
if (l2 /= 4 ) error stop
print*, l3
if (l3 /= 4 ) error stop
print*, l4
if (l4 /= 0 ) error stop

print*, len_trim(line1)
if (len_trim(line1) /= 18 ) error stop
print*, len_trim(line2)
if (len_trim(line2) /= 4 ) error stop
print*, len_trim(line3)
if (len_trim(line3) /= 4 ) error stop
print*, len_trim(line4)
if (len_trim(line4) /= 0 ) error stop

print*, l5
if (l5(1) /= 3 ) error stop
if (l5(2) /= 3 ) error stop
if (l5(3) /= 2 ) error stop
print*, l6
if (l6(1) /= 0 ) error stop
if (l6(2) /= 2 ) error stop
if (l6(3) /= 1 ) error stop
if (l6(4) /= 1 ) error stop

print*, len_trim(arr1(1))
if (len_trim(arr1(1)) /= 3 ) error stop
print*, len_trim(arr1(2))
if (len_trim(arr1(2)) /= 3 ) error stop
print*, len_trim(arr1(3))
if (len_trim(arr1(3)) /= 2 ) error stop
print*, len_trim(arr2(1))
if (len_trim(arr2(1)) /= 0 ) error stop
print*, len_trim(arr2(2))
if (len_trim(arr2(2)) /= 1 ) error stop
print*, len_trim(arr2(3))
if (len_trim(arr2(3)) /= 1 ) error stop


end program

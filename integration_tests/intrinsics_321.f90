program intrinsics_241
character(9):: hello = "  hello  "
character(7):: world = "  world"
character(6):: this = "this  "
character(4):: empty = "    "
character:: line = ''
character(5), parameter :: hello1 = trim("  hello  ")
character(5), parameter :: world1 = trim("  world")
character(4), parameter :: this1 = trim("this  ")

print*, hello1
if (hello1 /= "  hel") error stop
print*, world1
if (world1 /= "  wor") error stop
print*, this1
if (this1 /= "this") error stop

print*, trim(hello)
if (trim(hello) /= "  hello") error stop
print*, trim(world)
if (trim(world) /= "  world") error stop
print*, trim(this)
if (trim(this) /= "this") error stop
print*, trim(empty)
if (trim(empty) /= "") error stop
print*, trim(line)
if (trim(line) /= "") error stop
end program

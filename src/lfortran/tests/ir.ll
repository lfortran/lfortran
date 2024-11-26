define i32 @main(i32 %0, i8** %1) {
.entry:
  %a = add i32 0, 1
  %b = add i64 0, 2
  %s = add i32 %a, %b
  ret i32 %s
}

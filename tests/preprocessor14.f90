program preprocessor14

#define X DEC(4 - 3)
#define DEC(n) (n)
#if X == 0
print *, "X == 0"
#elif X == 1
print *, "X == 1"
#elif X == 2
print *, "X == 2"
#else
print *, "X other"
#endif

#undef X
#define X (4 - 3)
#if X == 0
print *, "X == 0"
#elif X == 1
print *, "X == 1"
#elif X == 2
print *, "X == 2"
#else
print *, "X other"
#endif

#undef X
#define X 4-3
#if X == 0
print *, "X == 0"
#elif X == 1
print *, "X == 1"
#elif X == 2
print *, "X == 2"
#else
print *, "X other"
#endif

#define X DEC(10 % 3)
#define DEC(n) (n)
#if X == 0
print *, "X == 0"
#elif X == 1
print *, "X == 1"
#elif X == 2
print *, "X == 2"
#else
print *, "X other"
#endif

#undef X
#define X (10 % 3)
#if X == 0
print *, "X == 0"
#elif X == 1
print *, "X == 1"
#elif X == 2
print *, "X == 2"
#else
print *, "X other"
#endif

#undef X
#define X 10%3
#if X == 0
print *, "X == 0"
#elif X == 1
print *, "X == 1"
#elif X == 2
print *, "X == 2"
#else
print *, "X other"
#endif

end program

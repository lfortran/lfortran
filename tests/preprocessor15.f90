program preprocessor15

#define X DEC(1 << 2)
#define DEC(n) (n)
#if X == 0
print *, "X == 0"
#elif X == 1
print *, "X == 1"
#elif X == 4
print *, "X == 4"
#else
print *, "X other"
#endif

#undef X
#define X (1 << 2)
#if X == 0
print *, "X == 0"
#elif X == 1
print *, "X == 1"
#elif X == 4
print *, "X == 4"
#else
print *, "X other"
#endif

#undef X
#define X 1<<2
#if X == 0
print *, "X == 0"
#elif X == 1
print *, "X == 1"
#elif X == 4
print *, "X == 4"
#else
print *, "X other"
#endif


#define X DEC(8 >> 2)
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
#define X (8 >> 2)
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
#define X 8>>2
#if X == 0
print *, "X == 0"
#elif X == 1
print *, "X == 1"
#elif X == 2
print *, "X == 2"
#else
print *, "X other"
#endif



#define X DEC((53 & 4) >> 2)
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
#define X ((53 & 4) >> 2)
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
#define X (53&4)>>2
#if X == 0
print *, "X == 0"
#elif X == 1
print *, "X == 1"
#elif X == 2
print *, "X == 2"
#else
print *, "X other"
#endif

#define X DEC((2 | 8) / 5)
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
#define X ((2 | 8) / 5)
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
#define X (2|8)/5
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

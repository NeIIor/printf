#include <stdint.h>

extern long MyPrintf(const char* format, ...);

int main() {
    int64_t count = 0;
    
    MyPrintf("0b%b;123456789A%n\n"
             "%c; %d; 0b%b; 0o%o; 0x%x; %%%%%%;\n"
             "lol %s heigh;\n"
             "%d %s %x %d%%%c%b\n", 
             -__LONG_MAX__+100, &count, 
             'c', __LONG_MAX__, 5, 16, 0xBADDEDD1l,
             "Стёпы Гизунова",
             -1, "love", 3802, 100, 33, 127);
    
    MyPrintf("count: %d\n", count);
    
    return 0;
}
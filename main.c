#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#define ERROR_HANDLE(call_func, ...)                                                                \
    do {                                                                                            \
        const int error_handler = call_func;                                                        \
        if (error_handler)                                                                          \
        {                                                                                           \
            fprintf(stderr, "Can't " #call_func ". Error: %d\n", error_handler);                    \
            __VA_ARGS__                                                                             \
            return error_handler;                                                                   \
        }                                                                                           \
    } while(0)

extern long printme(const char* const format, ...);

int main()
{
    printf("This program print smth with asm func:\n");


    int64_t count = 0;
    ERROR_HANDLE(MyPrintf("0b%b;123456789A%n\n"
                          "%c; %d; 0b%b; 0o%o; 0x%x; %%%%%%;\n"
                          "lol %s heigh;\n"
                          "%d %s %x %d%%%c%b\n", 
                         -__LONG_MAX__+100, &count, 'c', __LONG_MAX__, 3, 16, 0xBADDEDD1l,
                         "Стёпы Гизунова",
                         -1, "love", 3802, 100, 33, 127)
    );
    ERROR_HANDLE(MyPrintf("count: %d\n", count););

    return EXIT_SUCCESS;
}
#ifndef BUILD_IN_H_
#define BUILD_IN_H_

#define BUILD_IN_FUNCTIONS_COUNT 1

#ifdef __GNUC__
#define EXPORT __attribute__((visibility("default")))
#else
#define EXPORT
#endif

EXPORT void print(const char* format, ...);

#endif // BUILD_IN_H_

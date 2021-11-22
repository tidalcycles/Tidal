#ifndef LINK_WRAPPER_H_INCLUDED
#define LINK_WRAPPER_H_INCLUDED

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#else
#include <stdbool.h>
#endif

// Define opaque structure
typedef struct _link_wrapper_t link_wrapper_t;

double link_wrapper_test();
link_wrapper_t* wrapper_create();
double beat_time(link_wrapper_t* link);

#ifdef __cplusplus
}
#endif

#endif // LINK_WRAPPER_H_INCLUDED

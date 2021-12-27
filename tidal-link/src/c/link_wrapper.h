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

__declspec(dllexport) double link_wrapper_test();
__declspec(dllexport) link_wrapper_t* wrapper_create();
__declspec(dllexport) double beat_time(link_wrapper_t* link);
__declspec(dllexport) double enable_link(link_wrapper_t* link);
__declspec(dllexport) void set_tempo_at_beat(link_wrapper_t* link, double tempo, double inBeats);

#ifdef __cplusplus
}
#endif

#endif // LINK_WRAPPER_H_INCLUDED

#include "link_wrapper.h"

#include "ableton/Link.hpp"
#include <chrono>

class LinkWrapper {
public:
    LinkWrapper()
        : link(120.0)
    {
        // link.enableStartStopSync(true);

    }

    double beatTime()
    {
      auto sessionState = link.captureAppSessionState();
      return(sessionState.beatAtTime(now(), 1));
    }

    std::chrono::microseconds now() const
    {
      return link.clock().micros();
    }

    void enable_link() {
        this->link.enable(true);
    }

private:
    ableton::Link link;
    // double quantum;
    double latency = 0.0;

};

__declspec(dllexport) link_wrapper_t* wrapper_create(void)
{
    LinkWrapper* t;

    try {
        t = new LinkWrapper();
    } catch (...) {
        return nullptr;
    }

    return (link_wrapper_t*)t;
}

__declspec(dllexport) double enable_link(link_wrapper_t* link) {
    try {
        ((LinkWrapper*)link)->enable_link();
        return 0.0;
    } catch (...) {
        return 1.0;
    }
}

__declspec(dllexport) double beat_time(link_wrapper_t* link) {
  return(((LinkWrapper*)link)->beatTime());
}

__declspec(dllexport) double link_wrapper_test() {
    return 54;
}

#include "link_wrapper.h"

#include "ableton/Link.hpp"
#include <chrono>

class LinkWrapper {
public:
    LinkWrapper()
        : link(120.0)
    {
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

private:
    ableton::Link link;
};

link_wrapper_t* wrapper_create(void)
{
    LinkWrapper* t;

    try {
        t = new LinkWrapper();
    } catch (...) {
        return nullptr;
    }

    return (link_wrapper_t*)t;
}

double beat_time(link_wrapper_t* link) {
  return(((LinkWrapper*)link)->beatTime());
}

double link_wrapper_test() {
    return 54;
}

#pragma once

#include <cstdint>
#include <emmintrin.h>
#include <x86intrin.h>
#include "itch_header_parser.hpp"
#include "itch_header_parser.hpp"

class Handler {
public:
    inline void handle(std::byte const * msg_start, ITCH::ItchHeader header, uint16_t header_size);

private:
    uint64_t last_timestamp = 0;
};

inline void Handler::handle(std::byte const * msg_start, ITCH::ItchHeader header, uint16_t header_size) {

}

#pragma once

#include <cstdint>
#include <emmintrin.h>
#include <x86intrin.h>
#include "itch_parser.hpp"

struct Handler {
    inline void handle(ITCH::AddOrderNoMpid msg, uint16_t size);
    inline void handle(ITCH::OrderCancel msg, uint16_t size);
    inline void handle(ITCH::OrderDelete msg, uint16_t size);
    inline void handle(ITCH::OrderReplace msg, uint16_t size);

    inline void handle(ITCH::SystemEvent msg, uint16_t size);
    inline void handle(ITCH::StockDirectory msg, uint16_t size);
    inline void handle(ITCH::TradingAction msg, uint16_t size);
    inline void handle(ITCH::RegSho msg, uint16_t size);
    inline void handle(ITCH::MarketParticipantPos msg, uint16_t size);
    inline void handle(ITCH::MwcbDeclineLevel msg, uint16_t size);
    inline void handle(ITCH::MwcbStatus msg, uint16_t size);
    inline void handle(ITCH::IpoQuotationPeriodUpd msg, uint16_t size);
    inline void handle(ITCH::LuldAuctionCollar msg, uint16_t size);
    inline void handle(ITCH::OperationalHalt msg, uint16_t size);

    inline void handle(ITCH::AddOrderMpid msg, uint16_t size);
    inline void handle(ITCH::OrderExecuted msg, uint16_t size);
    inline void handle(ITCH::OrderExecutedPrice msg, uint16_t size);

    inline void handle(ITCH::TradeMessageNonCross msg, uint16_t size);
    inline void handle(ITCH::TradeMessageCross msg, uint16_t size);
    inline void handle(ITCH::BrokenTrade msg, uint16_t size);
    inline void handle(ITCH::Noii msg, uint16_t size);
    inline void handle(ITCH::DirectListingCapitalRaise msg, uint16_t size);

    void handle_after();
    void handle_before();
    void reset();

    uint64_t total_messages = 0;
    unsigned aux_start, aux_end;

    uint64_t t0;
};

inline void Handler::handle_before() {}
inline void Handler::handle_after() {}

#define DEF_HANDLER(T) \
inline void Handler::handle(T msg, uint16_t size) { \
    asm volatile("" : : "r,m"(msg)); \
}

DEF_HANDLER(ITCH::AddOrderNoMpid)
DEF_HANDLER(ITCH::OrderCancel)
DEF_HANDLER(ITCH::OrderDelete)
DEF_HANDLER(ITCH::OrderReplace)
DEF_HANDLER(ITCH::SystemEvent)
DEF_HANDLER(ITCH::StockDirectory)
DEF_HANDLER(ITCH::TradingAction)
DEF_HANDLER(ITCH::RegSho)
DEF_HANDLER(ITCH::MarketParticipantPos)
DEF_HANDLER(ITCH::MwcbDeclineLevel)
DEF_HANDLER(ITCH::MwcbStatus)
DEF_HANDLER(ITCH::IpoQuotationPeriodUpd)
DEF_HANDLER(ITCH::LuldAuctionCollar)
DEF_HANDLER(ITCH::OperationalHalt)
DEF_HANDLER(ITCH::AddOrderMpid)
DEF_HANDLER(ITCH::OrderExecuted)
DEF_HANDLER(ITCH::OrderExecutedPrice)
DEF_HANDLER(ITCH::TradeMessageNonCross)
DEF_HANDLER(ITCH::TradeMessageCross)
DEF_HANDLER(ITCH::BrokenTrade)
DEF_HANDLER(ITCH::Noii)
DEF_HANDLER(ITCH::DirectListingCapitalRaise)

#undef DEF_HANDLER

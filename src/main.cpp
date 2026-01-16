#include <atomic>
#include <iostream>
#include <stdexcept>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <thread>
#include <x86intrin.h>

#include "itch_parser.hpp"
#include "spsc_buffer.hpp"

std::atomic<bool> consume;
constexpr size_t _2MB = 2*1024*1024;

void reader(SPSCBuffer& ring_buffer, const std::string& itch_file_path) {
    int fd = open(itch_file_path.data(), O_RDONLY);
    struct stat st;
    if(fstat(fd, &st) < 0) {
        std::cerr << "Fstat failed" << '\n';
        std::abort();
    }

    size_t size = st.st_size;

    void* ptr = mmap(nullptr, size, PROT_READ, MAP_PRIVATE, fd, 0);
    if (ptr == MAP_FAILED) {
        std::cerr << "Map failed" << '\n';
        std::abort();
    }

    madvise(ptr, size, MADV_WILLNEED | MADV_SEQUENTIAL);
    std::byte* src = static_cast<std::byte*>(ptr);

    for (size_t i = 0; i < size; i += _2MB) {
        size_t len = std::min(_2MB, size - i);
        while (!ring_buffer.try_write({&src[i], len})) {
            _mm_pause();
        }

        madvise(src + i, len, MADV_DONTNEED);
    }

    consume.store(false, std::memory_order_release);

    munmap(ptr, size);
    close(fd);
}

void consumer(SPSCBuffer& ring_buffer) {
    auto buffer = std::make_unique<std::byte[]>(_2MB);
    std::span dst(buffer.get(), _2MB);

    while (true) {
        size_t read = ring_buffer.read(dst);

        if (!read) {
            if (!consume.load(std::memory_order_acquire)) {
                break;
            }
            _mm_pause();
            continue;
        }

        for (size_t i = 0; i < read; ++i) {
            asm volatile("" ::: "memory");
        }
    }
}

int main(int argc, char** argv) {
    if (argc < 2) {
        throw std::runtime_error("Usage: ./run [path to itch file]");
    }

    std::string itch_file_path = argv[1];
    SPSCBuffer ring_buffer;
    consume.store(true, std::memory_order_relaxed);

    auto reader_thread = std::thread([&ring_buffer, &itch_file_path]() {
        reader(ring_buffer, itch_file_path);
    });

    auto consumer_thread = std::thread([&ring_buffer]() {
        consumer(ring_buffer);
    });

    reader_thread.join();
    consumer_thread.join();

    return 0;
}

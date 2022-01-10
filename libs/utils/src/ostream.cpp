/*
 * Copyright (C) 2019 The Android Open Source Project
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#include <utils/ostream.h>
#include <utils/compiler.h>

#include <algorithm>
#include <stdarg.h>
#include <string>

namespace utils::io {

ostream::~ostream() = default;

// don't allocate any memory before we actually use the log because one of these is created
// per thread.
ostream::Buffer::Buffer() noexcept = default;

ostream::Buffer::~Buffer() noexcept {
    // note: on Android pre r14, thread_local destructors are not called
    free(buffer);
}

void ostream::Buffer::advance(ssize_t n) noexcept {
    if (n > 0) {
        size_t written = n < size ? size_t(n) : size;
        curr += written;
        size -= written;
    }
}

void ostream::Buffer::reserve(size_t newSize) noexcept {
    size_t offset = curr - buffer;
    if (buffer == nullptr) {
        buffer = (char*)malloc(newSize);
    } else {
        buffer = (char*)realloc(buffer, newSize);
    }
    assert(buffer);
    capacity = newSize;
    curr = buffer + offset;
    size = capacity - offset;
}

void ostream::Buffer::reset() noexcept {
    // aggressively shrink the buffer
    if (capacity > 1024) {
        free(buffer);
        buffer = (char*)malloc(1024);
        capacity = 1024;
    }
    curr = buffer;
    size = capacity;
}


std::pair<char*, size_t> ostream::Buffer::grow(size_t s) noexcept {
    if (UTILS_UNLIKELY(size < s)) {
        size_t used = curr - buffer;
        size_t newCapacity = std::max(size_t(32), used + (s * 3 + 1) / 2); // 32 bytes minimum
        reserve(newCapacity);
        assert(size >= s);
    }
    return { curr, size };
}

const char* ostream::getFormat(ostream::type t) const noexcept {
    switch (t) {
        case type::SHORT:       return mShowHex ? "0x%hx"  : "%hd";
        case type::USHORT:      return mShowHex ? "0x%hx"  : "%hu";
        case type::CHAR:        return "%c";
        case type::UCHAR:       return "%c";
        case type::INT:         return mShowHex ? "0x%x"   : "%d";
        case type::UINT:        return mShowHex ? "0x%x"   : "%u";
        case type::LONG:        return mShowHex ? "0x%lx"  : "%ld";
        case type::ULONG:       return mShowHex ? "0x%lx"  : "%lu";
        case type::LONG_LONG:   return mShowHex ? "0x%llx" : "%lld";
        case type::ULONG_LONG:  return mShowHex ? "0x%llx" : "%llu";
        case type::DOUBLE:      return "%f";
        case type::LONG_DOUBLE: return "%Lf";
    }
}

UTILS_NOINLINE
ostream& ostream::print(const char* format, ...) noexcept {
    va_list args0;
    va_list args1;

    // figure out how much size to we need
    va_start(args0, format);
    va_copy(args1, args0);
    ssize_t s = vsnprintf(nullptr, 0, format, args0);
    va_end(args0);

    // grow the buffer to the needed size
    Buffer& buf = getBuffer();
    auto [curr, size] = buf.grow(s + 1); // +1 to include the null-terminator

    // print into the buffer
    vsnprintf(curr, size, format, args1);

    // advance the buffer
    buf.advance(s);

    va_end(args1);

    return *this;
}

ostream& ostream::operator<<(short value) noexcept {
    const char* format = getFormat(type::SHORT);
    return print(format, value);
}

ostream& ostream::operator<<(unsigned short value) noexcept {
    const char* format = getFormat(type::USHORT);
    return print(format, value);
}

ostream& ostream::operator<<(char value) noexcept {
    const char* format = getFormat(type::CHAR);
    return print(format, value);
}

ostream& ostream::operator<<(unsigned char value) noexcept {
    const char* format = getFormat(type::UCHAR);
    return print(format, value);
}

ostream& ostream::operator<<(int value) noexcept {
    const char* format = getFormat(type::INT);
    return print(format, value);
}

ostream& ostream::operator<<(unsigned int value) noexcept {
    const char* format = getFormat(type::UINT);
    return print(format, value);
}

ostream& ostream::operator<<(long value) noexcept {
    const char* format = getFormat(type::LONG);
    return print(format, value);
}

ostream& ostream::operator<<(unsigned long value) noexcept {
    const char* format = getFormat(type::ULONG);
    return print(format, value);
}

ostream& ostream::operator<<(long long value) noexcept {
    const char* format = getFormat(type::LONG_LONG);
    return print(format, value);
}

ostream& ostream::operator<<(unsigned long long value) noexcept {
    const char* format = getFormat(type::ULONG_LONG);
    return print(format, value);
}

ostream& ostream::operator<<(float value) noexcept {
    return operator<<((double)value);
}

ostream& ostream::operator<<(double value) noexcept {
    const char* format = getFormat(type::DOUBLE);
    return print(format, value);
}

ostream& ostream::operator<<(long double value) noexcept {
    const char* format = getFormat(type::LONG_DOUBLE);
    return print(format, value);
}

ostream& ostream::operator<<(bool value) noexcept {
    return operator<<(value ? "true" : "false");
}

ostream& ostream::operator<<(const char* string) noexcept {
    return print("%s", string);
}

ostream& ostream::operator<<(const unsigned char* string) noexcept {
    return print("%s", string);
}

ostream& ostream::operator<<(const void* value) noexcept {
    return print("%p", value);
}

ostream& ostream::hex() noexcept {
    mShowHex = true;
    return *this;
}

ostream& ostream::dec() noexcept {
    mShowHex = false;
    return *this;
}

} // namespace utils::io

#pragma once

//hashset (Robin Hood implementation)
//
//search: O(1) average; O(n) worst
//insert: O(1) average; O(n) worst
//remove: O(1) average; O(n) worst
//
//requirements:
//  auto T::hash() const -> uint;
//  auto T::operator==(const T&) const -> bool;

#include <nall/bit.hpp>
#include <nall/maybe.hpp>
#include <nall/stdint.hpp>
#include <nall/vector.hpp>

namespace nall {

template<typename T>
struct hashset {
  struct entry_t {
    T* ptr = nullptr;
    uint hash = 0;
    explicit operator bool() const { return ptr != nullptr; }
    auto operator==(const entry_t& source) const -> bool {
      return hash == source.hash && *ptr == *source.ptr;
    }
  };

  hashset() = default;
  hashset(uint length) : length(bit::round(length)) {}
  hashset(const hashset& source) { operator=(source); }
  hashset(hashset&& source) { operator=(move(source)); }
  ~hashset() { reset(); }

  auto operator=(const hashset& source) -> hashset& {
    reset();
    if(source.pool) {
      for(uint n : range(source.length)) if(source.pool[n]) insert(*source.pool[n].ptr);
    }
    return *this;
  }

  auto operator=(hashset&& source) -> hashset& {
    reset();
    pool = source.pool;
    length = source.length;
    count = source.count;
    source.pool = nullptr;
    source.length = 8;
    source.count = 0;
    return *this;
  }

  explicit operator bool() const { return count; }
  auto capacity() const -> uint { return length; }
  auto size() const -> uint { return count; }

  auto reset() -> void {
    if(pool) {
      for(uint n : range(length)) {
        if(pool[n]) {
          delete pool[n].ptr;
          pool[n].ptr = nullptr;
        }
      }
      delete pool;
      pool = nullptr;
    }
    length = 8;
    count = 0;
  }

  auto reserve(uint size) -> void {
    //ensure all items will fit into pool (with <= 50% load) and amortize growth
    auto oldEntries = entries();
    delete pool;

    length = bit::round(max(size, count << 1));
    pool = new entry_t[length]();

    count = 0;
    if(pool) for(auto& entry : oldEntries) insert(*entry.ptr);
  }

  auto find(const T& value) -> maybe<T&> {
    const uint hash = value.hash();
    if(pool) for(uint i = 0; i < length; i++) {
      uint index = mask(hash + i);
      entry_t& entry = pool[index];
      if(!entry) break;
      if(entry.hash == hash && *entry.ptr == value) return *entry.ptr;

      if(i) {
        uint entry_relative = mask(index - entry.hash);
        if(entry_relative < i) break;
      }
    }
    return nothing;
  }

  auto insert(const T& value) -> maybe<T&> {
    if(!pool) pool = new entry_t[length]();
    //double pool size when load is >= 50%
    if(count >= (length >> 1)) reserve(length << 1);

    auto ptr = new T(value);
    const uint hash = value.hash();

    auto heldPtr = ptr;
    uint heldHash = hash;
    for(uint i = 0; i < length; i++) {
      uint index = mask(hash + i);
      entry_t& entry = pool[index];
      if(!entry) { count++; entry = {heldPtr, heldHash}; return *ptr; }
      if(entry.hash == heldHash && *entry.ptr == *heldPtr) return *heldPtr;

      if(i) {
        uint entry_relative = mask(index - entry.hash);
        if(entry_relative < i) {
          swap(pool[index].ptr, heldPtr);
          swap(pool[index].hash, heldHash);
        }
      }
    }

    //FIXME: this code should be unreachable, but if it isn't, then
    //       it's deleting the least-recently used value and not the inserted one!
    delete heldPtr;
    return nothing;
  }

  auto remove(const T& value) -> bool {
    const uint hash = value.hash();
    if(pool) for(uint i = 0; i < length; i++) {
      uint index = mask(hash + i);
      entry_t& entry = pool[index];
      if(!entry) break;

      if(entry.hash == hash && *entry.ptr == value) {
        delete entry.ptr;

        for(uint j = 1; j < length; j++) {
          uint other = mask(hash + i + j);
          entry_t& entry2 = pool[other];
          if(!entry2) break;
          uint entry_relative = mask(other - entry2.hash);
          if(!entry_relative) break;
          pool[index] = pool[other];
          index = other;
        }

        count--;
        pool[index] = entry_t();
        return true;
      }

      if(i) {
        uint entry_relative = mask(index - entry.hash);
        if(entry_relative < i) break;
      }
    }

    return false;
  }

  struct base_iterator {
    auto operator!=(const base_iterator& source) const -> bool { return position != source.position; }

    auto operator++() -> base_iterator& {
      if(++position >= size) { position = size; return *this; }
      queue.takeFirst();
      return *this;
    }

    base_iterator(const hashset& source, uint position) : source(source), position(position) {
      //queue = source.entries();
      if(source.pool && source.count) {
        queue.reserve(source.count);
        for(uint n : range(source.length)) if(source.pool[n]) queue.append(source.pool[n]);
      }
      size = queue.size();
    }

  protected:
    const hashset& source;
    uint position, size;
    vector<entry_t> queue;
  };

  struct iterator : base_iterator {
    iterator(const hashset& source, uint position) : base_iterator(source, position) {}
    auto operator*() const -> T& { return *base_iterator::queue.first().ptr; }
  };

  auto begin() -> iterator { return iterator(*this, 0); }
  auto end() -> iterator { return iterator(*this, size()); }

  struct const_iterator : base_iterator {
    const_iterator(const hashset& source, uint position) : base_iterator(source, position) {}
    auto operator*() const -> const T& { return *base_iterator::queue.first().ptr; }
  };

  auto begin() const -> const const_iterator { return const_iterator(*this, 0); }
  auto end() const -> const const_iterator { return const_iterator(*this, size()); }

  auto inline mask(const uint n) /*const*/ -> const uint {
    return n & (length - 1);
  }

  auto items() -> vector<T> {
    vector<T> vec;
    if(!pool || !count) return vec;

    vec.reserve(count);
    for(uint n : range(length)) if(pool[n]) vec.append(*pool[n].ptr);
    return vec;
  }

protected:
  entry_t* pool = nullptr;
  uint length = 8;  //length of pool
  uint count = 0;   //number of objects inside of the pool

  auto entries() -> vector<entry_t> {
    vector<entry_t> vec;
    if(!pool || !count) return vec;

    vec.reserve(count);
    for(uint n : range(length)) if(pool[n]) vec.append(pool[n]);
    return vec;
  }
};

}

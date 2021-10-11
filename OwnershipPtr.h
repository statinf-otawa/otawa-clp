/*
 *  This file contains a implementation of an ownership-aware pointer type for C++
 *	This file is a part of own-ptr
 *	Copyright (c) 2021, Zhenyu Bai
 *
 *	own-ptr is free software; you can redistribute it and/or modify
 *	it under the terms of the GNU General Public License as published by
 *	the Free Software Foundation; either version 2 of the License, or
 *	(at your option) any later version.
 *
 *	own-ptr is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *	GNU General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public License
 *	along with OTAWA; if not, write to the Free Software
 *	Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
 *	02110-1301  USA
 */

#ifndef OWNERSHIPPTR_H
#define OWNERSHIPPTR_H

#include <elm/assert.h>

namespace elm {
    template<class T>
    class OwnedPtr;

    template<class T>
    class BorrowedPtr {
    public:
        BorrowedPtr() : _ptr(nullptr) {};

        BorrowedPtr(const BorrowedPtr &) = default;

        // Cast From OwnedPtr is possible
        BorrowedPtr(const OwnedPtr<T> &t) : _ptr(t._ptr) {};

        // Construction from native pointer is possible
        BorrowedPtr(T *ptr) : _ptr(ptr) {};

        // Do not delete the pointer when destructed
        ~BorrowedPtr() = default;

        // Disable heap allocation/free
        void *operator new(unsigned long count) = delete;

        void *operator new[](unsigned long count) = delete;

        void operator delete(void *ptr) = delete;

        void operator delete[](void *ptr) = delete;

        inline explicit operator bool() const { return _ptr == nullptr; };

        // Accessing
        inline T &operator*() {
            ASSERTP(!operator bool(), "nullptr! Probably you are using a moved owned pointer or an uninitialised pointer");
            return *_ptr;
        }

        inline T *operator->() {
            ASSERTP(!operator bool(), "nullptr! Probably you are using a moved owned pointer or an uninitialised pointer");
            return _ptr;
        };

        // Const versions
        inline const T &operator*() const {
            ASSERTP(!operator bool(), "nullptr! Probably you are using a moved owned pointer or an uninitialised pointer");
            return *_ptr;
        };

        inline const T *operator->() const {
            ASSERTP(!operator bool(), "nullptr! Probably you are using a moved owned pointer or an uninitialised pointer");
            return _ptr;
        };

        // Cast to the pointer
        inline explicit operator const T *() const { return _ptr; };

        inline explicit operator T *() { return _ptr; };

        // Comparing just like native pointers
        inline bool operator==(const OwnedPtr<T> &t) const { return _ptr == t._ptr; };

        inline bool operator!=(const OwnedPtr<T> &t) const { return _ptr != t._ptr; };
    private:
        T *_ptr;
    };

    template<class T>
    class OwnedPtr {
    public:
        friend class BorrowedPtr<T>;

        OwnedPtr() : _ptr(nullptr) {};

        // By rvalue reference, requires an empty constructor and a copy-assignement of T;
        //OwnedPtr(T&& t):_ptr(new T()){*_ptr = t;};
        // To be more clear that a heap allocation occurred, one can use this method
        //static OwnedPtr newFrom(T&& t){
        //    OwnedPtr op;
        //    op._ptr = new T();
        //    *op._ptr = t;
        //    return op;
        //};



        // Construction from native pointer is possible
        OwnedPtr(T *t) : _ptr(t) {};

        // Create an owned pointer from an existing one, ownership is transferred
        OwnedPtr(OwnedPtr<T> &&t) : _ptr(t._ptr) { if (this != &t) t._ptr = nullptr; };

        OwnedPtr(OwnedPtr<T> &t) : _ptr(t._ptr) { if (this != &t) t._ptr = nullptr; };

        ~OwnedPtr() { delete _ptr; };

        // Disable heap allocation/free, the pointer is freed when last owner is destroyed
        void *operator new(unsigned long count) = delete;

        void *operator new[](unsigned long count) = delete;

        void operator delete(void *ptr) = delete;

        void operator delete[](void *ptr) = delete;

        inline explicit operator bool() const { return _ptr == nullptr; };

        // Transfer the ownership
        OwnedPtr<T> &operator=(OwnedPtr<T> &t) noexcept {
            if (&t != this)
                t._ptr = nullptr;
            return *this;
        }

        OwnedPtr<T> &operator=(OwnedPtr<T> &&t) noexcept {
            if (&t != this)
                t._ptr = nullptr;
            return *this;
        }

        // Accessing
        inline T &operator*() {
            ASSERTP(!operator bool(), "nullptr! Probably you are using a moved owned pointer or an uninitialised pointer");
            return *_ptr;
        }

        inline T *operator->() {
            ASSERTP(!operator bool(), "nullptr! Probably you are using a moved owned pointer or an uninitialised pointer");
            return _ptr;
        };

        // Const versions
        inline const T &operator*() const {
            ASSERTP(!operator bool(), "nullptr! Probably you are using a moved owned pointer or an uninitialised pointer");
            return *_ptr;
        };

        inline const T *operator->() const {
            ASSERTP(!operator bool(), "nullptr! Probably you are using a moved owned pointer or an uninitialised pointer");
            return _ptr;
        };

        // Cast to the pointer
        inline explicit operator const T *() const { return (const T *) _ptr; };

        inline explicit operator T *() { return _ptr; };

        // Comparing just like native pointers
        inline bool operator==(const OwnedPtr<T> &t) const { return _ptr == t._ptr; };

        inline bool operator!=(const OwnedPtr<T> &t) const { return _ptr != t._ptr; };

    private:
        T *_ptr;
    };
}

#endif //OWNERSHIPPTR_H

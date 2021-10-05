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



template<class T> class OwnedPtr;

template<class T>
class BorrowedPtr{
public:
    BorrowedPtr():_ptr(nullptr){};
    BorrowedPtr(const BorrowedPtr&)=default;

    // Cast From OwnedPtr is possible
    BorrowedPtr(const OwnedPtr<T>& t):_ptr(t._ptr){};

    // Construction from native pointer is possible
    BorrowedPtr(T* ptr):_ptr(ptr){};

    // Do not delete the pointer when destructed
    ~BorrowedPtr()= default;

    // Disable heap allocation/free
    void* operator new (unsigned long count)=delete;
    void* operator new[] (unsigned long count)=delete;
    void operator delete  ( void* ptr )=delete;
    void operator delete[]( void* ptr )=delete;

    inline bool isNull(){return _ptr== nullptr;};

    // Access operations, the cast operator is not provided to limit its behavior
    inline T& operator*(){ ASSERTP(!isNull(), "nullptr! Probably you are using a moved owned pointer or an uninitialised pointer"); return *_ptr;};
    inline T* operator->(){ ASSERTP(!isNull(), "nullptr! Probably you are using a moved owned pointer or an uninitialised pointer"); return _ptr;};
private:
    T* _ptr;
};

template<class T>
class OwnedPtr{
public:
    friend class BorrowedPtr<T>;
    OwnedPtr():_ptr(nullptr){};

    // Construction from native pointer is possible
    OwnedPtr(T* t):_ptr(t){};

    // Create an owned pointer from an existing one, ownership is transferred
    OwnedPtr(OwnedPtr<T>& t):_ptr(t._ptr){t._ptr = nullptr;};

    ~OwnedPtr(){delete _ptr;};

    // Disable heap allocation/free, the pointer is freed when last owner is destroyed
    void* operator new (unsigned long count)=delete;
    void* operator new[] (unsigned long count)=delete;
    void operator delete  ( void* ptr )=delete;
    void operator delete[]( void* ptr )=delete;

    inline bool isNull(){return _ptr== nullptr;};

    // Transfer the ownership
    OwnedPtr<T>& operator=(OwnedPtr<T>& t) noexcept {
        if (&t == this)
            t._ptr = nullptr;
        return *this;
    }

    // Access operations, the cast operator is not provided to limit its behavior
    inline T& operator*(){ ASSERTP(!isNull(), "nullptr! Probably you using an uninitialised pointer"); return *_ptr;};
    inline T* operator->(){ ASSERTP(!isNull(), "nullptr! Probably you are using an uninitialised pointer"); return _ptr;};

private:
    T* _ptr;
};

#endif //OWNERSHIPPTR_H

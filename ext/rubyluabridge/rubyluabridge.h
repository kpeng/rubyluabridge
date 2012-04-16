/*
 * RubyLuaBridge
 * 
 * Licensed under the BSD License:
 * 
 * Copyright (c) 2007, Evan Wies
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *     * Neither the name of 'neomantra' nor the
 *       names of its contributors may be used to endorse or promote products
 *       derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY Evan Wies ``AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL Evan Wies BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#ifndef NEOMANTRA_RUBYLUABRIDGE_H_
#define NEOMANTRA_RUBYLUABRIDGE_H_

#include <lua.h>
#include <ruby.h>

#include <boost/shared_ptr.hpp>

// Version Information
#define RUBYLUABRIDGE_VERSION       ("0.7")
#define RUBYLUABRIDGE_VERSION_NUM   (000700)


// shared_ptr deleter which invokes lua_close
struct lua_close_deleter
{
    void operator()(lua_State* L)
    {
        lua_close(L);
    }
};

/// Struct for our Ruby Lua::State.
/// Manages the lifetime of a lua_State using a shared_ptr
struct rlua_State
{
    lua_State* getState() { return Lstate.get(); }

    boost::shared_ptr<lua_State> Lstate; /// The lua_State we are wrapping
};


/// Struct for our Ruby Lua::Object.
/// 
/// We use it to persist Lua object beyond its lifetime on the Lua stack.
///
/// It uses Lua's "standard" reference system:
///     http://www.lua.org/manual/5.1/manual.html#luaL_ref
/// We follow the convention of using Lua's registry to store the references.
struct rlua_RefObject
{
    lua_State* getState() { return Lstate.get(); }

    boost::shared_ptr<lua_State> Lstate; /// The lua_State where the reference is held.
    int                          Lref;   /// The reference id
    VALUE                        Rstate; /// The Ruby Lua::State where this ref came from
};


#endif // NEOMANTRA_RUBYLUABRIDGE_H_

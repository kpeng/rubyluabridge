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

#include <assert.h>
extern "C" {
#include <lua.h>
#include <lualib.h>
#include <lauxlib.h>
}
#include "rubyluabridge.h"
#include "st.h"

// Debug Scaffolding
#ifdef RLB_DEBUG
#define RLB_DEBUG_PRINT(fmt, ...)  printf(fmt, __VA_ARGS__)
#else
#define RLB_DEBUG_PRINT(fmt, ...)
#endif


/*****************************************************************************/
//
// Global Variables and Typedefs
//
/*****************************************************************************/

VALUE mLua;
VALUE cLua_State;
VALUE cLua_RefObject;
VALUE cLua_Table;        // derives from Lua::RefObject


/// A place to stash error messages
#define RUBYLUABRIDGE_ERROR_BUFFER_SIZE    4096
char gszErrorBuffer[RUBYLUABRIDGE_ERROR_BUFFER_SIZE];


/// WE DON"T DO THIS YET BUT PROBABLY WILL
/// In general, I'm doing a little cheating.
/// Since cLua_State is just a wrapper around a lua_State*,
/// and always can always extract the lua_State
/// meaning, take advantage the lua_State is ALWAYS there...
///



/*****************************************************************************/
//
// Library Infrastructure
//
/*****************************************************************************/

// Fwd: Ruby Hash iterator, inserting elements to a table
static int _rhash_to_table_iter_func( VALUE rkey, VALUE rvalue, lua_State* L );


/// Copies the top of the stack to gszErrorBuffer,
/// pops the top of the stack, and returns the address to gszErrorBuffer.
/// This is useful for error handling functions.
char* pop_error_to_buffer( lua_State* L )
{
    const char* szerr = lua_tostring( L, -1 );
    strncpy( gszErrorBuffer, szerr, RUBYLUABRIDGE_ERROR_BUFFER_SIZE );
    lua_pop(L, 1);
    return gszErrorBuffer;
}


/// return whether the value at a given index is "callable",
/// meaning that we can try to invoke it like a method
int is_callable( lua_State* L, int idx )
{
    // functions are callable
    if ( lua_type(L, idx) == LUA_TFUNCTION )
        return 1;

    // check if it is supports the __call metamethod
    if ( !lua_getmetatable(L, idx) )  // no metatable?
        return 0;

    lua_pushstring( L, "__call" );
    lua_rawget(L, -2);
    int callable = (lua_isnil(L, -1) == 0);

    lua_pop( L, 2 );  // remove metatable and metafield
    return callable;
}


/// return whether the value at a given index is "indexable",
/// that means will doing a table-like lookup not crash
int is_indexable( lua_State* L, int idx )
{
    // tables are obviously indexable
    if ( lua_type(L, idx) == LUA_TTABLE )
        return 1;

    // check if it is supports the __index metamethod
    if ( !lua_getmetatable(L, idx) )  // no metatable?
        return 0;

    lua_pushstring( L, "__index" );
    lua_rawget(L, -2);
    int indexable = (lua_isnil(L, -1) == 0);

    lua_pop( L, 2 );  // remove metatable and metafield
    return indexable;
}


/// return whether the value at a given index is "new_indexable",
/// that means will doing a table-like lookup not crash
int is_new_indexable( lua_State* L, int idx )
{
    // tables are obviously indexable
    if ( lua_type(L, idx) == LUA_TTABLE )
        return 1;

    // check if it is supports the __index metamethod
    if ( !lua_getmetatable(L, idx) )  // no metatable?
        return 0;

    lua_pushstring( L, "__newindex" );
    lua_rawget(L, -2);
    int new_indexable = (lua_isnil(L, -1) == 0);

    lua_pop( L, 2 );  // remove metatable and metafield
    return new_indexable;
}


/// marshals the given stack value to Ruby
/// leaves the stack unchanged
VALUE marshal_lua_to_ruby( VALUE Rstate, lua_State* L, int idx )
{
    int ltype = lua_type( L, idx );
    switch ( ltype )
    {
    // primitive types -> marshal directly
    case LUA_TNONE:
    case LUA_TNIL:
        return Qnil;
    case LUA_TBOOLEAN:
        return lua_toboolean(L,idx) ? Qtrue : Qfalse;
    case LUA_TNUMBER:
        return rb_float_new( lua_tonumber(L,idx) );
    case LUA_TSTRING:
        {
        size_t len = 0;
        const char* str = lua_tolstring(L, idx, &len);
        return rb_str_new( str, len );
        }

    // complex types
    // these are stored in Lua::RefObjects
    // except tables, which are stored in Lua::Table
    case LUA_TLIGHTUSERDATA:
    case LUA_TFUNCTION:
    case LUA_TUSERDATA:
    case LUA_TTHREAD:
    case LUA_TTABLE:
        {
        // make a Lua reference for this
        lua_pushvalue( L, idx );
        int ref = luaL_ref( L, LUA_REGISTRYINDEX );
        RLB_DEBUG_PRINT( "ref created:  L:%p   r:%d\n", L, ref);
        // create a new lua_Ref object holding it
        VALUE args[2] = { Rstate, INT2NUM(ref) };
        VALUE res = rb_class_new_instance( 2, args,
                (ltype == LUA_TTABLE) ? cLua_Table : cLua_RefObject );
        return res;
        }
    default:
        return Qnil;
    }
}


/// marshals the given Ruby value to Lua, leaving it on the top
/// returns a non-zero error code on failure
int marshal_ruby_to_lua_top( lua_State* L, VALUE val )
{
    int rtype = TYPE(val);
    switch ( rtype )
    {
    case T_NONE:   lua_pushnil( L );                     break;
    case T_NIL:    lua_pushnil( L );                     break;
    case T_TRUE:   lua_pushboolean( L, 1 );              break;
    case T_FALSE:  lua_pushboolean( L, 0 );              break;
    case T_FIXNUM: lua_pushnumber( L, FIX2INT(val) );    break;
    case T_BIGNUM: lua_pushnumber( L, NUM2DBL(val) );    break;
    case T_FLOAT:
        lua_pushnumber( L, (lua_Number)RFLOAT(val)->value );
        break;
    case T_STRING:
        lua_pushlstring( L, RSTRING(val)->ptr, RSTRING(val)->len );
        break;
    case T_SYMBOL:
        lua_pushstring( L, rb_id2name( SYM2ID(val) ) );
        break;
    // Hash becomes a table
    case T_HASH:
        {
        lua_newtable( L );
        rb_hash_foreach( val, (int (*)(ANYARGS))_rhash_to_table_iter_func, VALUE(L) );
        break;
        }
    // Array becomes a table-array.
    // note in Lua, the first index is 1, whereas in Ruby the first index is 0
    case T_ARRAY:
        {
        long i;
        VALUE array = val;
        lua_newtable( L );
        for ( i = 0; i < RARRAY(array)->len; i++ )
        {
            marshal_ruby_to_lua_top( L, RARRAY(array)->ptr[i] );
            lua_rawseti( L, -2, i+1 );    // Lua is 1-based
        }
        break;
        }
    case T_OBJECT:
        {
        // if it's a Lua::RefObject, put it on the stack
        if ( rb_obj_is_kind_of(val, cLua_RefObject) == Qtrue )
        {
            rlua_RefObject* pRefObject;
            Data_Get_Struct( val, rlua_RefObject, pRefObject );
            if ( pRefObject->getState() != L )
            {
                // TODO: handle this better
                rb_warning( "Marshalling Lua::RefObject between two different states.  Pushing nil." );
                break;
            }
            lua_rawgeti( L, LUA_REGISTRYINDEX, pRefObject->Lref );
            break;
        }
        // else, flow through to default...
        }
    default:
        // just throw the Ruby object up as a light userdata
        lua_pushlightuserdata( L, (void*)val );
        break;
    }
    return 0;
}


// dispatches method_missing
//
// assumes that the Lua object (RefObject) is on the top of the stack
// leaves the stack clean and returns the corresponding value
//
// an = at the end of the key makes it an assignment
// an ! at the end of the key makes it a method call with self
// an _ at the end of the key forces it as a method call without self
// nothing at the end of the key will make a function call,
//      unless there are no arguments in which case it simply returns obj[key]
//
VALUE rlua_method_missing_dispatch( lua_State* L, const char* key, VALUE Rstate, int argc, VALUE* argv )
{
    int keylen = strlen( key );

    // if method ends "=", then it's an assignment
    if ( key[keylen-1] == '=' )
    {
        assert( argc >= 2 );
        lua_pushlstring( L, key, keylen-1 );  // strip off =
        marshal_ruby_to_lua_top( L, argv[1] );
        lua_settable( L, -3 );

        lua_pop( L, 1 );
        return argv[1];
    }

    //
    // otherwise read the index and return the value or invoke the function
    //

    // determine any special treatment and retrieve the value
    int pushself  = (key[keylen-1] == '!');
    int forcecall = (key[keylen-1] == '_');   // keylen-1 to strip ! or _
    lua_pushlstring( L, key, (pushself || forcecall) ? keylen-1 : keylen );
    lua_gettable( L, -2 );

    // if there are no arguments (besides self) and the method doesn't ends with a special char
    // and it is not a function, then just return the indexed value.
    if ( !pushself && !forcecall && argc == 1 && (lua_type(L,-1) != LUA_TFUNCTION) )
    {
        // marshal the result to Ruby
        VALUE result = marshal_lua_to_ruby( Rstate, L, -1 );
        lua_pop( L, 2 );
        return result;
    }

    //
    // otherwise, we're gonna pcall the indexed value
    //

    // make sure it is callable
    if ( !is_callable(L,-1) )
    {
        int ltype = lua_type( L, -1 );
        lua_pop( L, 2 );
        rb_raise( rb_eRuntimeError,
            "Value is not callable (not a function and no __call metamethod), ltype: %d, key: %s",
            ltype, key );
    }

    // push the arguments on the stack
    int args_bottom = lua_gettop(L) - 1;
    if ( pushself )
        lua_pushvalue( L, -2 ); // push self as arg with method!
    int i;
    for ( i = 1; i < argc; ++i )
        marshal_ruby_to_lua_top( L, argv[i] );

    // pcall and handle errors
    int err = lua_pcall( L, (pushself ? argc : argc-1), LUA_MULTRET, 0 );
    if ( err == LUA_ERRRUN ) {
        lua_remove( L, -2 );
        rb_raise( rb_eRuntimeError, pop_error_to_buffer(L) );
    } else if ( err == LUA_ERRMEM ) {
        lua_remove( L, -2 );
        rb_raise( rb_eNoMemError, pop_error_to_buffer(L) );
    } else if ( err == LUA_ERRERR ) {
        lua_remove( L, -2 );
        rb_raise( rb_eFatal, pop_error_to_buffer(L) );
    }

    // if there is one result, return that alone
    // otherwise put them in an Array
    int args_top = lua_gettop(L);
    int nres = args_top - args_bottom;
    if ( nres == 1 )
    {
        // marshal the result to Ruby
        VALUE result = marshal_lua_to_ruby( Rstate, L, -1 );
        lua_pop( L, 2 );
        return result;
    }
    else
    {
        int li, ri;
        VALUE ary_result = rb_ary_new2( nres );
        for ( li = args_bottom+1, ri = 0; li <= args_top; ++li, ++ri )
            rb_ary_store( ary_result, ri, marshal_lua_to_ruby(Rstate, L, li) );
        lua_pop( L, 1+nres );
        return ary_result;
    }
}


/// Ruby Hash iterator, mapping pairs into a table
static int _rhash_to_table_iter_func( VALUE rkey, VALUE rvalue, lua_State* L )
{
    marshal_ruby_to_lua_top( L, rkey );
    marshal_ruby_to_lua_top( L, rvalue );
    lua_settable( L, -3 );
    return ST_CONTINUE;
}


// copied from:
//   http://www.lua.org/source/5.1/lbaselib.c.html#luaB_tostring
static int rluaB_tostring(lua_State *L)
{
    luaL_checkany(L, 1);
    if (luaL_callmeta(L, 1, "__tostring"))  /* is there a metafield? */
        return 1;  /* use its value */
    switch (lua_type(L, 1))
    {
    case LUA_TNUMBER:
        lua_pushstring(L, lua_tostring(L, 1));
        break;
    case LUA_TSTRING:
        lua_pushvalue(L, 1);
        break;
    case LUA_TBOOLEAN:
        lua_pushstring(L, (lua_toboolean(L, 1) ? "true" : "false"));
        break;
    case LUA_TNIL:
        lua_pushliteral(L, "nil");
        break;
    default:
        lua_pushfstring(L, "%s: %p", luaL_typename(L, 1), lua_topointer(L, 1));
        break;
    }
    return 1;
}


// mapping of Lua Standard Library names to their registration functions
static const luaL_Reg rubylua_lualibs[] = {
    {"", luaopen_base},
    {LUA_LOADLIBNAME, luaopen_package},
    {LUA_TABLIBNAME, luaopen_table},
    {LUA_IOLIBNAME, luaopen_io},
    {LUA_OSLIBNAME, luaopen_os},
    {LUA_STRLIBNAME, luaopen_string},
    {LUA_MATHLIBNAME, luaopen_math},
    {LUA_DBLIBNAME, luaopen_debug},
    {NULL, NULL}
};


/// loads the specified standard Lua library
void load_std_library_by_name( lua_State* L, const char* libname )
{
    // special-case:  base
    const luaL_Reg* libreg = NULL;
    if ( !strcmp(libname, "base") )
        libreg = &rubylua_lualibs[0];
    // otherwise search for it
    else
    {
        for ( libreg = &rubylua_lualibs[1]; libreg->func; libreg++ )
            if ( !strcmp(libname, libreg->name) )
                break;
    }

    // register if found
    if ( libreg && libreg->func )
    {
        lua_pushcfunction( L, libreg->func );
        lua_pushstring( L, libreg->name );
        lua_call( L, 1, 0 );
    }
}



/*****************************************************************************

 Document-class: Lua::State

 The Ruby representation of a lua_State.

*****************************************************************************/

// Forward declaration
static VALUE rlua_State_loadlibs( VALUE self, VALUE libs );

/* call-seq:
 *        Lua::State.new( options )
 *
 * Creates a new Lua::State.
 *
 * _options_ is a hash of options.  If no _options_ are specified, the default is { :loadlibs => :all }.
 *
 * <b>loadlibs:</b>
 * Invokes Lua::State.__loadlibs on the new Lua::State, passing the value of :loadlibs.
 *
 * Raises NoMemoryError if the state cannot be allocated, or ArgumentError if the value of :loadlibs
 * is invalid.
 */
static VALUE rlua_State_initialize( int argc, VALUE* argv, VALUE self )
{
    rlua_State* pRLState;
    Data_Get_Struct( self, rlua_State, pRLState );

    // create new Lua state
    pRLState->Lstate.reset( luaL_newstate(), lua_close_deleter() );
    if ( !pRLState->getState() )
        rb_raise( rb_eNoMemError, "lua_State memory allocation failed" );

    RLB_DEBUG_PRINT( "state   init: ptr:%p   L:%p\n", pRLState, pRLState->getState() );

    // if there is no arguments (or nil first value), load all
    if ( argc == 0 || NIL_P(argv[0]) ) {
        luaL_openlibs( pRLState->getState() );
        return self;
    }
    // otherwise, it has to be a hash
    Check_Type( argv[0], T_HASH );

    // process "loadlibs"
    VALUE libs = rb_hash_aref( argv[0], ID2SYM(rb_intern("loadlibs")) );
    rlua_State_loadlibs( self, libs ); // OK if nil

    return self;
}


/// free the lua_State, create with lua_State_alloc
static void rlua_State_free( rlua_State* pRLState )
{
        RLB_DEBUG_PRINT( "state free  :  ptr:%p   L:%p\n", pRLState, pRLState->getState() );
}


/// allocate a new Lua::State
/// this actually performs the luaL_newstate allocation
static VALUE rlua_State_alloc( VALUE klass )
{
    rlua_State* pRLState = new rlua_State();
    if ( !pRLState )
        rb_raise( rb_eNoMemError, "Out of memory when allocating rlua_State" );
    RLB_DEBUG_PRINT( "state malloc: ptr:%p\n", pRLState );
    // wrap it inside a Ruby object
    VALUE obj = Data_Wrap_Struct( klass, NULL, rlua_State_free, pRLState );
    return obj;
}


/* call-seq:
 *        Lua::State.__state -> Lua::State
 *
 * Returns this Lua::State itself.
 *
 * Introduced for parallelism with Lua::RefObject.__state.
 */
static VALUE rlua_State_state( VALUE self )
{
    return self;
}


/* call-seq:
 *        Lua::State.__top -> int
 *
 * Return the absolute position of the top of the lua_State's stack.
 *
 * This is mainly for debugging/testing purposes.
 */
static VALUE rlua_State_top( VALUE self )
{
    rlua_State* pRLState;
    Data_Get_Struct( self, rlua_State, pRLState );
    int top = lua_gettop( pRLState->getState() );
    return INT2NUM( top );
}


/* call-seq:
 *        Lua::State.__globals -> Lua::Table
 *
 * Returns the globals table of this Lua::State.
 * It is an instance of the Lua::Table class.
 */
static VALUE rlua_State_globals( VALUE self )
{
    rlua_State* pRLState;
    Data_Get_Struct( self, rlua_State, pRLState );
    lua_State* L = pRLState->getState();

    lua_pushvalue( L, LUA_GLOBALSINDEX );
    VALUE result = marshal_lua_to_ruby( self, L, -1 );
    lua_pop( L, 1 );
    return result;
}


/* call-seq:
 *        Lua::State.__registry -> Lua::Table
 *
 * Returns the registry table of this Lua::State.
 * It is an instance of the Lua::Table class.
 *
 * As the Lua Registry is intended for C extensions
 * and the Lua reference system, be careful modifying
 * values stored in this table.
 */
static VALUE rlua_State_registry( VALUE self )
{
    rlua_State* pRLState;
    Data_Get_Struct( self, rlua_State, pRLState );
    lua_State* L = pRLState->getState();

    lua_pushvalue( L, LUA_REGISTRYINDEX );
    VALUE result = marshal_lua_to_ruby( self, L, -1 );
    lua_pop( L, 1 );
    return result;
}


/* call-seq:
 *      Lua::State.__loadlib( libs )
 *
 * Loads the specified Lua standard libraries into the Lua::State.
 *
 * If _libs_ is not specified, all libraries are loaded.  Otherwise, if _libs_ is a symbol, that library
 * is loaded.  Special symbols are :all, which loads all libraries, and :none which loads no libraries.
 * If _libs_ is an Array, all symbols in the Array are loaded (in the order specified); in this case,
 * :all and :none are ignored; an empty Array will load no libraries.  If none of the above fits,
 * an ArgumentError is raised.
 *
 * Supported libraries are:
 * - base
 * - package
 * - table
 * - io
 * - os
 * - string
 * - math
 * - debug
 */
static VALUE rlua_State_loadlibs( VALUE self, VALUE libs )
{
    rlua_State* pRLState;
    Data_Get_Struct( self, rlua_State, pRLState );
    lua_State* L = pRLState->getState();

    // if it is empty or :all, load all
    // if it is a symbol, load that the lib it matches
    // if it is :none, load none
    // if it is an array, load all its symbols (:all is ignored here)
    // otherwise, load none
    if ( NIL_P(libs) )
    {
        luaL_openlibs( L );
    }
    else if ( TYPE(libs) == T_SYMBOL )
    {
        const char* libname = rb_id2name( SYM2ID(libs) );
        if ( !strcmp(libname, "all") )
            luaL_openlibs( L );
        else if ( !strcmp(libname, "none") )
        {}  // load none on :none
        else
            load_std_library_by_name( L, libname );
    }
    else if ( TYPE(libs) == T_ARRAY )
    {
        int i;
        for ( i = 0; i < RARRAY(libs)->len; i++ )
        {
            VALUE entry = RARRAY(libs)->ptr[i];
            if ( TYPE(entry) == T_SYMBOL )
            {
                const char* libname = rb_id2name( SYM2ID(entry) );
                load_std_library_by_name( L, libname );
            }
        }
    }
    else
        rb_raise( rb_eArgError, "loadlibs must be Nil, a Symbol, or an Array of symbols" );

    return self;
}


/* call-seq:
 *        Lua::State.eval -> result
 *
 * Evaluates the passed string in the Lua::State.
 *
 * Returns the first value returned by the evaluation.
 */
static VALUE rlua_State_eval( VALUE self, VALUE str )
{
    // verify and marshal Ruby args to C
    rlua_State* pRLState;
    Data_Get_Struct( self, rlua_State, pRLState );
    SafeStringValue(str);
    lua_State* L = pRLState->getState();

    // process the string to a chunk
    int err = luaL_loadbuffer( L, RSTRING(str)->ptr, RSTRING(str)->len, "Lua::State.eval" );
    if ( err == LUA_ERRMEM )
        rb_raise( rb_eNoMemError, pop_error_to_buffer(L) );
    else if ( err == LUA_ERRSYNTAX )
        rb_raise( rb_eSyntaxError, pop_error_to_buffer(L) );

    // pcall the chunk, returning only a single argument
    // TODO: error handler with stack traceback
    // TODO: it would be nice to have it configurable whether to print the traceback
    // TODO: hmmm... the err handler could even be in Ruby?
    err = lua_pcall( L, 0, 1, 0 );
    if ( err == LUA_ERRRUN )
        rb_raise( rb_eRuntimeError, pop_error_to_buffer(L) );
    else if ( err == LUA_ERRMEM )
        rb_raise( rb_eNoMemError, pop_error_to_buffer(L) );
    else if ( err == LUA_ERRERR )
        rb_raise( rb_eFatal, pop_error_to_buffer(L) );

    // marshal the result to Ruby
    VALUE result = marshal_lua_to_ruby( self, L, -1 );
    lua_pop( L, 1 );
    return result;
}


/* call-seq:
 *        Lua::State.eval_mult -> Array
 *
 * Evaluates the passed string in the Lua::State.
 *
 * Returns the all the return values in a Ruby array (with the first result first).
 * If there are no results, an empty array is returned.
 */
static VALUE rlua_State_eval_mult( VALUE self, VALUE str )
{
    // verify and marshal Ruby args to C
    rlua_State* pRLState;
    Data_Get_Struct( self, rlua_State, pRLState );
    SafeStringValue(str);
    lua_State* L = pRLState->getState();
    int args_bottom = lua_gettop(L);

    // process the string to a chunk
    int err = luaL_loadbuffer( L, RSTRING(str)->ptr, RSTRING(str)->len, "Lua::State.eval" );
    if ( err == LUA_ERRMEM )
        rb_raise( rb_eNoMemError, pop_error_to_buffer(L) );
    else if ( err == LUA_ERRSYNTAX )
        rb_raise( rb_eSyntaxError, pop_error_to_buffer(L) );

    // pcall the chunk, returning only a single argument
    // TODO: error handler with stack traceback
    // TODO: it would be nice to have it configurable whether to print the traceback
    // TODO: hmmm... the err handler could even be in Ruby?
    err = lua_pcall( L, 0, LUA_MULTRET, 0 );
    if ( err == LUA_ERRRUN )
        rb_raise( rb_eRuntimeError, pop_error_to_buffer(L) );
    else if ( err == LUA_ERRMEM )
        rb_raise( rb_eNoMemError, pop_error_to_buffer(L) );
    else if ( err == LUA_ERRERR )
        rb_raise( rb_eFatal, pop_error_to_buffer(L) );

    // marshal the result to Ruby
    int args_top = lua_gettop(L);
    int nres = args_top - args_bottom;
    int li, ri;
    VALUE ary_res = rb_ary_new2( nres );
    for ( li = args_bottom+1, ri = 0; li <= args_top; ++li, ++ri )
        rb_ary_store( ary_res, ri, marshal_lua_to_ruby(self, L, li) );
    lua_pop( L, nres );
    return ary_res;
}


/* call-seq:
 *        Lua::State.method_missing -> result
 *
 * This method is called by Ruby when it sees an Object can't handle a message.
 * We use it to dispatch to Lua, attempting a lookup of that value in the Lua::State's global table.
 *
 * If the method name has an '<tt>=</tt>' at the end, it is treated as an assignment,
 * in which case it assigns the first value.  It returns that value for chaining.
 *
 * The first argument is the symbol of the message name, the rest are its args.
 */
VALUE rlua_State_method_missing( int argc, VALUE* argv, VALUE self )
{
    rlua_State* pRLstate;
    Data_Get_Struct( self, rlua_State, pRLstate );
    lua_State* L = pRLstate->getState();

    Check_Type( argv[0], T_SYMBOL );
    ID methodid = SYM2ID( argv[0] );
    const char* key = rb_id2name( methodid );

    lua_pushvalue( L, LUA_GLOBALSINDEX );
    return rlua_method_missing_dispatch( L, key, self, argc, argv );
}


/* call-seq:
 *        Lua::State[key] -> value
 *
 * Returns the value indexed at _key_ in the Lua::State's globals table.
 */
VALUE rlua_State_getindex( VALUE self, VALUE key )
{
    rlua_State* pRLstate;
    Data_Get_Struct( self, rlua_State, pRLstate );
    lua_State* L = pRLstate->getState();

    marshal_ruby_to_lua_top( L, key );
    lua_gettable( L, LUA_GLOBALSINDEX );

    // marshal the result to Ruby
    VALUE result = marshal_lua_to_ruby( self, L, -1 );
    lua_pop( L, 1 );
    return result;
}


/* call-seq:
 *        Lua::State[key] = value -> value
 *
 * Assigns _value_ to be indexed at _key_ in the Lua::State's globals table.
 * Returns the value for chaining.
 */
VALUE rlua_State_setindex( VALUE self, VALUE key, VALUE val )
{
    rlua_State* pRLstate;
    Data_Get_Struct( self, rlua_State, pRLstate );
    lua_State* L = pRLstate->getState();

    marshal_ruby_to_lua_top( L, key );
    marshal_ruby_to_lua_top( L, val );
    lua_settable( L, LUA_GLOBALSINDEX );

    return val;    // return val for chaining
}


/* call-seq:
 *        Lua::State.new_table_at key -> Lua::Table
 *
 * Creates a new table at the given key.  Returns the new table.
 */
VALUE rlua_State_new_table_at( VALUE self, VALUE key )
{
    rlua_State* pRLstate;
    Data_Get_Struct( self, rlua_State, pRLstate );
    lua_State* L = pRLstate->getState();

    marshal_ruby_to_lua_top( L, key );
    lua_newtable( L );

    VALUE result = marshal_lua_to_ruby( self, L, -1 );
    lua_settable( L, LUA_GLOBALSINDEX );

    return result;
}


/* call-seq:
 *        Lua::State.indexable? -> true
 *
 * Returns whether Lua:State is indexable (via __index), which it is.
 * This is to provide consistency with Lua::RefObject interface.
 */
VALUE rlua_State_is_indexable( VALUE self )
{
        return Qtrue;
}


/* call-seq:
 *        Lua::State.new_indexable? -> true
 *
 * Returns whether Lua:State can create new indices (via __newindex), which it can.
 * This is to provide consistency with Lua::RefObject interface.
 */
VALUE rlua_State_is_new_indexable( VALUE self )
{
        return Qtrue;
}


/* call-seq:
 *        Lua::State.callable? -> true
 *
 * Returns whether Lua:State is callable (like via __cal), which it is not..
 * This is to provide consistency with Lua::RefObject interface.
 */
VALUE rlua_State_is_callable( VALUE self )
{
        return Qfalse;
}


/*****************************************************************************

 Document-class: Lua::RefObject

 The Ruby representation of non-primitive objects in Lua.

*****************************************************************************/

/* call-seq:
 *        Lua::RefObject.new -> result
 *
 * Initializes a new Lua::RefObject.
 *
 * THIS METHOD IS NOT INTENDED TO BE CALLED BY CLIENTS.
 *
 * TODO: Can we enforce the hiding of this?  Only Lua module members should access it.
 */
static VALUE rlua_RefObject_initialize( VALUE self, VALUE Rstate, VALUE RluaRef )
{
    rlua_RefObject* pRef;
    Data_Get_Struct( self, rlua_RefObject, pRef );

    RLB_DEBUG_PRINT( "ref init:  self:%d   Rstate:%p  TYPE(Rstate):%d  RluaRef:%d TYPE(RluaRef):%d  luaRef:%d\n",
        (unsigned long)self, (unsigned long)Rstate, (unsigned long)TYPE(Rstate),
        (unsigned long)RluaRef, (unsigned long)TYPE(RluaRef), NUM2INT(RluaRef) );

    pRef->Rstate   = Rstate;
    pRef->Lref     = NUM2INT(RluaRef);

    rlua_State* pRState;
    Data_Get_Struct( Rstate, rlua_State, pRState );
    pRef->Lstate  = pRState->Lstate;

    return self;
}


/// free the Lua::RefObject, created with lua_RefObject_alloc
static void rlua_RefObject_free( rlua_RefObject* pRefObject )
{
        RLB_DEBUG_PRINT( "ref free:  ptr:%p   ref:%d  L:%p\n", pRefObject, pRefObject->Lref, pRefObject->getState() );
    assert( pRefObject != NULL );
    luaL_unref( pRefObject->getState(), LUA_REGISTRYINDEX, pRefObject->Lref );

    delete pRefObject;
}


/// allocate a new Lua::RefObject
static VALUE rlua_RefObject_alloc( VALUE klass )
{
    rlua_RefObject* pRef = new rlua_RefObject();
    if ( !pRef )
        rb_raise( rb_eNoMemError, "Out of memory when allocating rlua_RefObject" );
//  pRef->Lstate = NULL;
    pRef->Lref   = LUA_NOREF;
    pRef->Rstate = Qnil;

    // wrap it inside a Ruby object
    //rlua_RefObject_mark
    VALUE obj = Data_Wrap_Struct( klass, NULL, rlua_RefObject_free, pRef );
    return obj;
}


/* call-seq:
 *        Lua::RefObject.__state -> Lua::State
 *
 * Returns the Lua::State this Lua::RefObject belongs to.
 */
static VALUE rlua_RefObject_state( VALUE self )
{
    rlua_RefObject* pRefObject;
    Data_Get_Struct( self, rlua_RefObject, pRefObject );

    return pRefObject->Rstate;
}


/* call-seq:
 *        Lua::RefObject.method_missing -> result
 *
 * This method is called by Ruby when it sees an Object can't handle a message.
 * We use it to dispatch to Lua, attempting a lookup of that value in the Lua::RefObject.
 *
 * The first argument is the symbol of the message name, the rest are its args.

 * If the method name has an <tt>=</tt> at the end, it is treated as an assignment,
 * in which case it assigns the first value.  It returns that value for chaining.
 *
 * If the method name has a '<tt>_</tt>' at the end, it is treated as an method invocation,
 * passing itself as a parameter.  This is to emulate the '<tt>:</tt>' token used in Lua.
 */
VALUE rlua_RefObject_method_missing( int argc, VALUE* argv, VALUE self )
{
    rlua_RefObject* pRefObject;
    Data_Get_Struct( self, rlua_RefObject, pRefObject );
    lua_State* L = pRefObject->getState();

    Check_Type( argv[0], T_SYMBOL );
    ID methodid = SYM2ID( argv[0] );
    const char* key = rb_id2name( methodid );

    lua_rawgeti( L, LUA_REGISTRYINDEX, pRefObject->Lref );
    if ( !is_indexable(L, -1) )
    {
        lua_pop(L, 1);
        rb_raise( rb_eRuntimeError, "Lua::RefObject not indexable, key='%s'", key );
    }

    return rlua_method_missing_dispatch( L, key, pRefObject->Rstate, argc, argv );
}


/* call-seq:
 *        Lua::RefObject.__length -> int
 *
 * Returns the 'length' of the RefObject.
 *
 * According to the {Lua manual}[http://www.lua.org/manual/5.1/manual.html#lua_objlen]:
 *    Returns the "length" of the value at the given acceptable index:
 *    for strings, this is the string length; for tables, this is the result
 *    of the length operator ('#'); for userdata, this is the size of the
 *    block of memory allocated for the userdata; for other values, it is 0.
 */
VALUE rlua_RefObject_length( VALUE self )
{
    rlua_RefObject* pRefObject;
    Data_Get_Struct( self, rlua_RefObject, pRefObject );
    lua_State* L = pRefObject->getState();

    lua_rawgeti( L, LUA_REGISTRYINDEX, pRefObject->Lref );
    size_t len = lua_objlen(L, -1);
    lua_pop( L, 1 );

    VALUE result = INT2NUM(len);
    return result;
}


/* call-seq:
 *        Lua::RefObject.__type -> int
 *
 * Returns the type id of the underlying Lua object.
 */
VALUE rlua_RefObject_type( VALUE self )
{
    rlua_RefObject* pRefObject;
    Data_Get_Struct( self, rlua_RefObject, pRefObject );
    lua_State* L = pRefObject->getState();

    lua_rawgeti( L, LUA_REGISTRYINDEX, pRefObject->Lref );
    size_t len = lua_type(L, -1);
    lua_pop( L, 1 );

    VALUE result = INT2NUM(len);
    return result;
}


/* call-seq:
 *      Lua::RefObject.__metatable -> Lua::Table
 *
 * Returns the metatable of the underlying Lua object.
 * Return nil if it has no metatable.
 */
VALUE rlua_RefObject_getmetatable( VALUE self )
{
    rlua_RefObject* pRefObject;
    Data_Get_Struct( self, rlua_RefObject, pRefObject );
    lua_State* L = pRefObject->getState();

    lua_rawgeti( L, LUA_REGISTRYINDEX, pRefObject->Lref );

    if (lua_getmetatable(L, -1) == 0)
        lua_pushnil( L );

    VALUE result = marshal_lua_to_ruby( pRefObject->Rstate, L, -1 );
    lua_pop( L, 2 );
    return result;
}


/* call-seq:
 *      Lua::RefObject.__metatable= Lua::Table -> Lua::Table
 *
 * Sets the metatable for this Lua::RefObject.
 * Returns the passed metatable.
 */
VALUE rlua_RefObject_setmetatable( VALUE self, VALUE mtable )
{
    rlua_RefObject* pRefObject;
    Data_Get_Struct( self, rlua_RefObject, pRefObject );
    lua_State* L = pRefObject->getState();

    lua_rawgeti( L, LUA_REGISTRYINDEX, pRefObject->Lref );

    marshal_ruby_to_lua_top( L, mtable );
    lua_setmetatable( L, -2 );

    lua_pop( L, 1 );
    return mtable;
}


/* call-seq:
 *        Lua::RefObject.[key] -> value
 *
 * Returns the value indexed at _key_ in the RefObject.
 *
 * Raises RuntimeError if the RefObject is not indexable.
 */
VALUE rlua_RefObject_getindex( VALUE self, VALUE key )
{
    rlua_RefObject* pRefObject;
    Data_Get_Struct( self, rlua_RefObject, pRefObject );
    lua_State* L = pRefObject->getState();

    lua_rawgeti( L, LUA_REGISTRYINDEX, pRefObject->Lref );
    if ( !is_indexable(L, -1) )
    {
        lua_pop( L, 1 );
        rb_raise( rb_eRuntimeError, "(getindex) Lua::RefObject not indexable" );
    }

    marshal_ruby_to_lua_top( L, key );
    lua_gettable( L, -2 );

    // marshal the result to Ruby
    VALUE result = marshal_lua_to_ruby( pRefObject->Rstate, L, -1 );
    lua_pop( L, 2 );
    return result;
}


/* call-seq:
 *        Lua::RefObject.[key] = value -> value
 *
 * Assigns _value_ to be indexed at _key_ in the RefObject.
 * Returns the value for chaining.
 *
 * Raises RuntimeError if the RefObject is not indexable.
 */
VALUE rlua_RefObject_setindex( VALUE self, VALUE key, VALUE val )
{
    rlua_RefObject* pRefObject;
    Data_Get_Struct( self, rlua_RefObject, pRefObject );
    lua_State* L = pRefObject->getState();

    lua_rawgeti( L, LUA_REGISTRYINDEX, pRefObject->Lref );
    if ( !is_indexable(L, -1) )
    {
        lua_pop( L, 1 );
        rb_raise( rb_eRuntimeError, "(setindex) Lua::RefObject not indexable" );
    }

    marshal_ruby_to_lua_top( L, key );
    marshal_ruby_to_lua_top( L, val );
    lua_settable( L, -3 );

    lua_pop( L, 1 );
    return val;    // return val for chaining
}


/* call-seq:
 *        Lua::RefObject.new_table_at key -> Lua::Table
 *
 * Creates a new table at the given key.  Returns the new table.
 *
 * Raises RuntimeError if the RefObject is not indexable.
 */
VALUE rlua_RefObject_new_table_at( VALUE self, VALUE key )
{
    rlua_RefObject* pRefObject;
    Data_Get_Struct( self, rlua_RefObject, pRefObject );
    lua_State* L = pRefObject->getState();

    lua_rawgeti( L, LUA_REGISTRYINDEX, pRefObject->Lref );
    if ( !is_indexable(L, -1) )
    {
        lua_pop( L, 1 );
        rb_raise( rb_eRuntimeError, "(setindex) Lua::RefObject not indexable" );
    }

    marshal_ruby_to_lua_top( L, key );
    lua_newtable( L );

    VALUE result = marshal_lua_to_ruby( pRefObject->Rstate, L, -1 );
    lua_settable( L, -3 );

    lua_pop( L, 1 );
    return result;
}


/* call-seq:
 *      Lua::RefObject.to_s -> string
 *
 * Invokes the Lua function {tostring}[http://www.lua.org/manual/5.1/manual.html#pdf-tostring]
 * on the object and returns the resulting string.
 */
VALUE rlua_RefObject_to_s( VALUE self )
{
    rlua_RefObject* pRefObject;
    Data_Get_Struct( self, rlua_RefObject, pRefObject );
    lua_State* L = pRefObject->getState();

    lua_rawgeti( L, LUA_REGISTRYINDEX, pRefObject->Lref );
    rluaB_tostring( L );

    VALUE result = marshal_lua_to_ruby( pRefObject->Rstate, L, -1 );

    lua_pop( L, 2 );
    return result;
}


/* call-seq:
 *        Lua::RefObject.indexable? -> true
 *
 * Returns whether Lua:State is indexable (via __index), which it is.
 * This is to provide consistency with Lua::RefObject interface.
 */
VALUE rlua_RefObject_is_indexable( VALUE self )
{
    rlua_RefObject* pRefObject;
    Data_Get_Struct( self, rlua_RefObject, pRefObject );
    lua_State* L = pRefObject->getState();

    lua_rawgeti( L, LUA_REGISTRYINDEX, pRefObject->Lref );
    VALUE result = is_indexable(L, -1) ? Qtrue : Qfalse;

    lua_pop( L, 1 );
    return result;
}


/* call-seq:
 *        Lua::RefObject.new_indexable? -> true
 *
 * Returns whether Lua:State can create new indices (via __newindex), which it can.
 * This is to provide consistency with Lua::RefObject interface.
 */
VALUE rlua_RefObject_is_new_indexable( VALUE self )
{
    rlua_RefObject* pRefObject;
    Data_Get_Struct( self, rlua_RefObject, pRefObject );
    lua_State* L = pRefObject->getState();

    lua_rawgeti( L, LUA_REGISTRYINDEX, pRefObject->Lref );
    VALUE result = is_new_indexable(L, -1) ? Qtrue : Qfalse;

    lua_pop( L, 1 );
    return result;
}


/* call-seq:
 *        Lua::State.indexable? -> true
 *
 * Returns whether Lua:State is callable (like via __cal), which it is not..
 * This is to provide consistency with Lua::RefObject interface.
 */
VALUE rlua_RefObject_is_callable( VALUE self )
{
    rlua_RefObject* pRefObject;
    Data_Get_Struct( self, rlua_RefObject, pRefObject );
    lua_State* L = pRefObject->getState();

    lua_rawgeti( L, LUA_REGISTRYINDEX, pRefObject->Lref );
    VALUE result = is_callable(L, -1) ? Qtrue : Qfalse;

    lua_pop( L, 1 );
    return result;
}



/*****************************************************************************

 Document-class: Lua::Table

 When Rua marshals a Lua table to Ruby, it instantiates it as a Lua::Table.

 This gives it some extra methods that aren't available to a regular Lua::RefObject.

*****************************************************************************/

/////////////////////////////////
///// LUA::TABLE CONVERSION

/* call-seq:
 *        Lua::Table.to_array -> Array
 *
 * Returns a Ruby Array of the (first, dense) integer keys in the Table.
 */
VALUE rlua_Table_to_array( VALUE self )
{
    rlua_RefObject* pRefObject;
    Data_Get_Struct( self, rlua_RefObject, pRefObject );
    lua_State* L = pRefObject->getState();

    lua_rawgeti( L, LUA_REGISTRYINDEX, pRefObject->Lref );
    int tablelen = lua_objlen( L, -1 );

    VALUE array = rb_ary_new2( tablelen );
    int i;
    for ( i = 1; i <= tablelen; ++i )
    {
        lua_rawgeti( L, -1, i );
        VALUE rvalue = marshal_lua_to_ruby( pRefObject->Rstate, L, -1 );
        rb_ary_push( array, rvalue );
        lua_pop( L, 1 );
    }

    lua_pop( L, 1 );
    return array;
}


/* call-seq:
 *        Lua::Table.to_hash -> Hash
 *
 * Returns a Ruby Hash of all pairs in the table.
 */
VALUE rlua_Table_to_hash( VALUE self )
{
    rlua_RefObject* pRefObject;
    Data_Get_Struct( self, rlua_RefObject, pRefObject );
    lua_State* L = pRefObject->getState();

    VALUE hash = rb_hash_new();

    // table is in the stack at index 't'
    lua_rawgeti( L, LUA_REGISTRYINDEX, pRefObject->Lref );
    int table = lua_gettop( L );
    lua_pushnil( L );  // first key
    while ( lua_next(L, table) != 0 )
    {
        // uses 'key' (at index -2) and 'value' (at index -1)
        VALUE rvalue = marshal_lua_to_ruby( pRefObject->Rstate, L, -1 );
        VALUE rkey   = marshal_lua_to_ruby( pRefObject->Rstate, L, -2 );
        rb_hash_aset( hash, rkey, rvalue );
        // removes 'value'; keeps 'key' for next iteration
        lua_pop( L, 1 );
    }

    lua_pop( L, 1 );
    return hash;
}


/////////////////////////////////
///// LUA::TABLE HASH ITERATION

/* call-seq:
 *      Lua::Table.each { |key,value| block } -> Lua::Table
 *      Lua::Table.each_pair { |key,value| block } -> Lua::Table
 *
 * Calls _block_ once for each key in _table_, passing the key and value as parameters.
 *
 * This goes over all pairs in the table.
 */
VALUE rlua_Table_each_pair( VALUE self )
{
    rlua_RefObject* pRefObject;
    Data_Get_Struct( self, rlua_RefObject, pRefObject );
    lua_State* L = pRefObject->getState();

    // table is in the stack at index 't'
    lua_rawgeti( L, LUA_REGISTRYINDEX, pRefObject->Lref );
    int table = lua_gettop( L );
    lua_pushnil( L );  // first key
    while ( lua_next(L, table) != 0 )
    {
        // uses 'key' (at index -2) and 'value' (at index -1)
        VALUE rvalue = marshal_lua_to_ruby( pRefObject->Rstate, L, -1 );
        VALUE rkey   = marshal_lua_to_ruby( pRefObject->Rstate, L, -2 );
        rb_yield_values( 2, rkey, rvalue );
        // removes 'value'; keeps 'key' for next iteration
        lua_pop( L, 1 );
    }

    lua_pop( L, 1 );
    return self;
}


/* call-seq:
 *      Lua::Table.each_key { |key| block } -> Lua::Table
 *
 * Calls _block_ once for each key in _table_, passing the key to the block as a parameter.
 *
 * This goes over all pairs in the table.
 */
VALUE rlua_Table_each_key( VALUE self )
{
    rlua_RefObject* pRefObject;
    Data_Get_Struct( self, rlua_RefObject, pRefObject );
    lua_State* L = pRefObject->getState();

    // table is in the stack at index 't'
    lua_rawgeti( L, LUA_REGISTRYINDEX, pRefObject->Lref );
    int table = lua_gettop( L );
    lua_pushnil( L );  // first key
    while ( lua_next(L, table) != 0 )
    {
        // uses 'key' (at index -2) and 'value' (at index -1)
        VALUE rkey   = marshal_lua_to_ruby( pRefObject->Rstate, L, -2 );
        rb_yield( rkey );
        // removes 'value'; keeps 'key' for next iteration
        lua_pop( L, 1 );
    }

    lua_pop( L, 1 );
    return self;
}


/* call-seq:
 *      Lua::Table.each_value { |value| block } -> Lua::Table
 *
 * Calls _block_ once for each key in _table_, passing the value to the block as a parameter.
 *
 * This goes over all pairs in the table.
 */
VALUE rlua_Table_each_value( VALUE self )
{
    rlua_RefObject* pRefObject;
    Data_Get_Struct( self, rlua_RefObject, pRefObject );
    lua_State* L = pRefObject->getState();

    // table is in the stack at index 't'
    lua_rawgeti( L, LUA_REGISTRYINDEX, pRefObject->Lref );
    int table = lua_gettop( L );
    lua_pushnil( L );  // first key
    while ( lua_next(L, table) != 0 )
    {
        // uses 'key' (at index -2) and 'value' (at index -1)
        VALUE rvalue = marshal_lua_to_ruby( pRefObject->Rstate, L, -1 );
        rb_yield( rvalue );
        // removes 'value'; keeps 'key' for next iteration
        lua_pop( L, 1 );
    }

    lua_pop( L, 1 );
    return self;
}


/////////////////////////
///// ARRAY ITERATION

/* call-seq:
 *      Lua::Table.each_ipair { |key,value| block } -> Lua::Table
 *
 * Calls _block_ once for each integer key in _table_, passing the key and value as parameters.
 *
 * This goes over all integer pairs in the table.  Similar to ipairs(), this
 * only touches the first #table integers, thus treating the table like a dense array.
 */
VALUE rlua_Table_each_ipair( VALUE self )
{
    rlua_RefObject* pRefObject;
    Data_Get_Struct( self, rlua_RefObject, pRefObject );
    lua_State* L = pRefObject->getState();

    lua_rawgeti( L, LUA_REGISTRYINDEX, pRefObject->Lref );
    int tablelen = lua_objlen( L, -1 );
    int i;
    for ( i = 1; i <= tablelen; ++i )
    {
        lua_rawgeti( L, -1, i );
        VALUE rvalue = marshal_lua_to_ruby( pRefObject->Rstate, L, -1 );
        rb_yield_values( 2, INT2NUM(i), rvalue );
        lua_pop( L, 1 );
    }

    lua_pop( L, 1 );

    return self;
}


/* call-seq:
 *      Lua::Table.each_index { |key| block } -> Lua::Table
 *      Lua::Table.each_ikey  { |key| block } -> Lua::Table
 *
 * Calls _block_ once for each integer key in _table_, passing the key to the block as a parameter.
 *
 * This goes over all integer pairs in the table.  Similar to ipairs(), this
 * only touches the first #table integers, thus treating the table like a dense array.
 */
VALUE rlua_Table_each_ikey( VALUE self )
{
    rlua_RefObject* pRefObject;
    Data_Get_Struct( self, rlua_RefObject, pRefObject );
    lua_State* L = pRefObject->getState();

    lua_rawgeti( L, LUA_REGISTRYINDEX, pRefObject->Lref );
    int tablelen = lua_objlen( L, -1 );
    int i;
    for ( i = 1; i <= tablelen; ++i )
    {
        lua_rawgeti( L, -1, i );
        rb_yield( INT2NUM(i) );
        lua_pop( L, 1 );
    }

    lua_pop( L, 1 );

    return self;
}


/* call-seq:
 *      Lua::Table.each_ivalue { |value| block } -> Lua::Table
 *
 * Calls _block_ once for each integer key in _table_, passing the value to the block as a parameter.
 *
 * This goes over all integer pairs in the table.  Similar to ipairs(), this
 * only touches the first #table integers, thus treating the table like a dense array.
 */
VALUE rlua_Table_each_ivalue( VALUE self )
{
    rlua_RefObject* pRefObject;
    Data_Get_Struct( self, rlua_RefObject, pRefObject );
    lua_State* L = pRefObject->getState();

    lua_rawgeti( L, LUA_REGISTRYINDEX, pRefObject->Lref );
    int tablelen = lua_objlen( L, -1 );
    int i;
    for ( i = 1; i <= tablelen; ++i )
    {
        lua_rawgeti( L, -1, i );
        VALUE rvalue = marshal_lua_to_ruby( pRefObject->Rstate, L, -1 );
        rb_yield( rvalue );
        lua_pop( L, 1 );
    }

    lua_pop( L, 1 );

    return self;
}



/*****************************************************************************/
//
// Extension Initialization
//
/*****************************************************************************/

extern "C" {
/*
  Document-module: Lua

  The module where all of the RubyLuaBridge components live.
*/
void Init_rubyluabridge()
{
    // Lua module
    mLua = rb_define_module( "Lua" );
    rb_define_const( mLua, "BRIDGE_VERSION",     rb_str_new2( RUBYLUABRIDGE_VERSION ) );
    rb_define_const( mLua, "BRIDGE_VERSION_NUM", INT2FIX( RUBYLUABRIDGE_VERSION_NUM ) );
    rb_define_const( mLua, "LUA_VERSION",        rb_str_new2( LUA_VERSION ) );
    rb_define_const( mLua, "LUA_RELEASE",        rb_str_new2( LUA_RELEASE ) );

    rb_define_const( mLua, "TNONE",              INT2FIX( LUA_TNONE ) );
    rb_define_const( mLua, "TNIL",               INT2FIX( LUA_TNIL ) );
    rb_define_const( mLua, "TBOOLEAN",           INT2FIX( LUA_TBOOLEAN ) );
    rb_define_const( mLua, "TLIGHTUSERDATA",     INT2FIX( LUA_TLIGHTUSERDATA ) );
    rb_define_const( mLua, "TNUMBER",            INT2FIX( LUA_TNUMBER ) );
    rb_define_const( mLua, "TSTRING",            INT2FIX( LUA_TSTRING ) );
    rb_define_const( mLua, "TTABLE",             INT2FIX( LUA_TTABLE ) );
    rb_define_const( mLua, "TFUNCTION",          INT2FIX( LUA_TFUNCTION ) );
    rb_define_const( mLua, "TUSERDATA",          INT2FIX( LUA_TUSERDATA ) );
    rb_define_const( mLua, "TTHREAD",            INT2FIX( LUA_TTHREAD ) );

    // Lua::State class
    cLua_State = rb_define_class_under( mLua, "State", rb_cObject );
    rb_define_alloc_func( cLua_State, rlua_State_alloc );
    rb_define_method( cLua_State, "initialize",     RUBY_METHOD_FUNC(rlua_State_initialize), -1 );
    rb_define_method( cLua_State, "method_missing", RUBY_METHOD_FUNC(rlua_State_method_missing), -1 );
    rb_define_method( cLua_State, "__state",        RUBY_METHOD_FUNC(rlua_State_state), 0 );
    rb_define_method( cLua_State, "__top",          RUBY_METHOD_FUNC(rlua_State_top), 0 );
    rb_define_method( cLua_State, "__globals",      RUBY_METHOD_FUNC(rlua_State_globals), 0 );
    rb_define_method( cLua_State, "__registry",     RUBY_METHOD_FUNC(rlua_State_registry), 0 );
    rb_define_method( cLua_State, "__loadlibs",     RUBY_METHOD_FUNC(rlua_State_loadlibs), 1 );
    rb_define_method( cLua_State, "new_table_at",   RUBY_METHOD_FUNC(rlua_State_new_table_at), 1 );
    rb_define_method( cLua_State, "[]",             RUBY_METHOD_FUNC(rlua_State_getindex), 1 );
    rb_define_method( cLua_State, "[]=",            RUBY_METHOD_FUNC(rlua_State_setindex), 2 );
    rb_define_method( cLua_State, "eval",           RUBY_METHOD_FUNC(rlua_State_eval), 1 );
    rb_define_method( cLua_State, "eval_mult",      RUBY_METHOD_FUNC(rlua_State_eval_mult), 1 );
    rb_define_method( cLua_State, "callable?",      RUBY_METHOD_FUNC(rlua_State_is_callable), 0 );
    rb_define_method( cLua_State, "indexable?",     RUBY_METHOD_FUNC(rlua_State_is_indexable), 0 );
    rb_define_method( cLua_State, "new_indexable?", RUBY_METHOD_FUNC(rlua_State_is_new_indexable), 0 );
// TODO: more methods!

//    rb_define_method( cLua_State, "execute",      lua_State_eval, 1 );
//    rb_define_method( cLua_State, "execute_mult", lua_State_eval_mult, 1 );

    // Lua::RefObject class
    cLua_RefObject = rb_define_class_under( mLua, "RefObject", rb_cObject );
    rb_define_alloc_func( cLua_RefObject, rlua_RefObject_alloc );
    rb_define_method( cLua_RefObject, "initialize",     RUBY_METHOD_FUNC(rlua_RefObject_initialize), 2 );
    rb_define_method( cLua_RefObject, "method_missing", RUBY_METHOD_FUNC(rlua_RefObject_method_missing), -1 );
    rb_define_method( cLua_RefObject, "__state",        RUBY_METHOD_FUNC(rlua_RefObject_state), 0 );
    rb_define_method( cLua_RefObject, "__length",       RUBY_METHOD_FUNC(rlua_RefObject_length), 0 );
    rb_define_method( cLua_RefObject, "__type",         RUBY_METHOD_FUNC(rlua_RefObject_type), 0 );
    rb_define_method( cLua_RefObject, "__metatable",    RUBY_METHOD_FUNC(rlua_RefObject_getmetatable), 0 );
    rb_define_method( cLua_RefObject, "__metatable=",   RUBY_METHOD_FUNC(rlua_RefObject_setmetatable), 1 );
    rb_define_method( cLua_RefObject, "[]",             RUBY_METHOD_FUNC(rlua_RefObject_getindex), 1 );
    rb_define_method( cLua_RefObject, "[]=",            RUBY_METHOD_FUNC(rlua_RefObject_setindex), 2 );
    rb_define_method( cLua_RefObject, "new_table_at",   RUBY_METHOD_FUNC(rlua_RefObject_new_table_at), 1 );
    rb_define_method( cLua_RefObject, "to_s",           RUBY_METHOD_FUNC(rlua_RefObject_to_s), 0 );
    rb_define_method( cLua_RefObject, "method_missing", RUBY_METHOD_FUNC(rlua_RefObject_method_missing), -1 );
    rb_undef_method(  cLua_RefObject, "type" );
    rb_undef_method(  cLua_RefObject, "id" );
    rb_define_method( cLua_RefObject, "callable?",      RUBY_METHOD_FUNC(rlua_RefObject_is_callable), 0 );
    rb_define_method( cLua_RefObject, "indexable?",     RUBY_METHOD_FUNC(rlua_RefObject_is_indexable), 0 );
    rb_define_method( cLua_RefObject, "new_indexable?", RUBY_METHOD_FUNC(rlua_RefObject_is_new_indexable), 0 );

    // Lua::Table class
    cLua_Table = rb_define_class_under( mLua, "Table", cLua_RefObject );
    rb_define_method( cLua_Table, "to_array",    RUBY_METHOD_FUNC(rlua_Table_to_array), 0 );
    rb_define_method( cLua_Table, "to_hash",     RUBY_METHOD_FUNC(rlua_Table_to_hash), 0 );
    rb_define_method( cLua_Table, "each_ipair",  RUBY_METHOD_FUNC(rlua_Table_each_ipair), 0 );
    rb_define_method( cLua_Table, "each_ikey",   RUBY_METHOD_FUNC(rlua_Table_each_ikey), 0 );
    rb_define_method( cLua_Table, "each_ivalue", RUBY_METHOD_FUNC(rlua_Table_each_ivalue), 0 );
    rb_define_method( cLua_Table, "each_pair",   RUBY_METHOD_FUNC(rlua_Table_each_pair), 0 );
    rb_define_method( cLua_Table, "each_key",    RUBY_METHOD_FUNC(rlua_Table_each_key), 0 );
    rb_define_method( cLua_Table, "each_value",  RUBY_METHOD_FUNC(rlua_Table_each_value), 0 );
    rb_define_alias(  cLua_Table, "each", "each_pair" );
    rb_define_alias(  cLua_Table, "each_index", "each_ikey" );
}

} // extern "C"


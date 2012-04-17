RubyLuaBridge
=============

RubyLuaBridge lets you access Lua from Ruby.  Eventually, support for accessing 
Ruby from Lua will be added.  This documentation assumes basic knowledge of
[Ruby](http://www.ruby-lang.org) and [Lua](http://www.lua.org).

This is a fork of the original BitBucket repository that packages the files into
a RubyGem.

Relevant web pages:

* [RubyLuaBridge Homepage](http://rubyluabridge.rubyforge.org/)
* [RubyForge Project](http://rubyforge.org/projects/rubyluabridge/)
* [BitBucket Repository](https://bitbucket.org/neomantra/rubyluabridge)

Installation
------------

This fork of RubyLuaBridge has packaged the files as a RubyGem.  To build the gem
and install:

    % gem build rubyluabridge.gemspec
    % gem install rubyluabridge-0.7.0 -- --with-lua-include=/usr/include/lua5.1 --with-lualib=lua5.1

You should replace the Lua configuration options with your appropriate platform's
locations.

Design Philosophy
-----------------

* Simplest marshalling possible
* Simplest API possible
* Do more complicated things in Lua itself, rather than exposing them!  For
  example, the coroutine library is not explicitly exposed because it can be
  handled all through `eval` and indexing: `l.coroutine.create(f)`

Ruby to Lua Type Mapping
------------------------

<table>
  <tr><td><strong>Ruby</strong></td><td><strong>Lua</strong></td></tr>
  <tr><td><code>nil</code></td><td><code>nil</code></td></tr>
  <tr><td><code>None</code></td><td><code>nil</code></td></tr>
  <tr><td><code>True</code></td><td><code>true</code></td></tr>
  <tr><td><code>False</code></td><td><code>false</code></td></tr>
  <tr><td><code>Fixnum</code></td><td><code>number</code></td></tr>
  <tr><td><code>Bignum</code></td><td><code>number</code></td></tr>
  <tr><td><code>Float</code></td><td><code>number</code></td></tr>
  <tr><td><code>String</code></td><td><code>string</code></td></tr>
  <tr><td><code>Symbol</code></td><td><code>string</code></td></tr>
  <tr><td><code>Hash</code></td><td><code>new Lua::Table</code> clone</td></tr>
  <tr><td><code>Array</code></td><td><code>new Lua::Table</code> clone</td></tr>
  <tr><td>Other</td><td><code>lightuserdata</code></td></tr>
</table>

Lua to Ruby Type Mapping
------------------------

<table>
  <tr><td><strong>Lua</strong></td><td><strong>Ruby</strong></td></tr>
  <tr><td><code>none</code></td><td><code>nil</code></td></tr>
  <tr><td><code>nil</code></td><td><code>nil</code></td></tr>
  <tr><td><code>true</code></td><td><code>True</code></td></tr>
  <tr><td><code>false</code></td><td><code>False</code></td></tr>
  <tr><td><code>number</code></td><td><code>Float</code></td></tr>
  <tr><td><code>string</code></td><td><code>String</code></td></tr>
  <tr><td><code>table</code></td><td><code>Lua::Table</code></td></tr>
  <tr><td><code>lightuserdata</code></td><td><code>Lua::RefObject</code></td></tr>
  <tr><td><code>function</code></td><td><code>Lua::RefObject</code></td></tr>
  <tr><td><code>userdata</code></td><td><code>Lua::RefObject</code></td></tr>
  <tr><td><code>thread</code></td><td><code>Lua::RefObject</code></td></tr>
</table>

To Do
-----

* Stack trace in error callback
* How to get some external `lua_State` there?
* How to deal with `.clone()` and `.dup()`?
* Accessing Ruby from Lua

Credits
-------

The following persons have contributed to RubyLuaBridge:

* Evan Wies (evan a neomantra d net)

RubyLuaBridge is inspired by, but not derived from:

* [Lunatic Python](http://labix.org/lunatic-python)
* [ruby-lua](http://raa.ruby-lang.org/project/ruby-lua)

Support
-------

* [RubyLuaBridge Homepage](http://rubyluabridge.rubyforge.org/)
* [RubyForge Project](http://rubyforge.org/projects/rubyluabridge/)

Download the latest sources from there.  Please use the mailing list and issue
tracking features as well.

I am particularly interested in problems you may have on various systems.  I have
tested it in Ubuntu Linux and OS X.

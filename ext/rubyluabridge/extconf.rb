
require 'mkmf'

def fail( str )
  STDERR << " extconf failed: #{str}\n"
  exit 1
end

if enable_config('debug')
    $CFLAGS = '-O0 -g -Wall '
else
    $CFLAGS = '-O3 -Wall'
end

if enable_config('rlb-debug')
    $CFLAGS += '-DRLB_DEBUG '
end

# link in C++ libraries
$LIBS << " -lstdc++ -lc"

nolua_msg = <<END_OF_MESSAGE
need liblua.

        Install the library or try one of the following options to extconf.rb:

        --with-lua-lib=/path/to/liblua/lib
        --with-lua-include=/path/to/liblua/include
        --with-lualib=name_of_lua_library

        --enable-debug will build it optimized and with debugging symbols
END_OF_MESSAGE

dir_config 'lua'

unless have_header('lua.h')
    fail nolua_msg
end

unless have_library('lua', 'lua_pushvalue')
    fail nolua_msg
end

create_makefile('rubyluabridge/rubyluabridge')

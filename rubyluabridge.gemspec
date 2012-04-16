Gem::Specification.new do |s|
    s.name        = 'rubyluabridge'
    s.version     = '0.7.0'
    s.date        = '2010-06-11'
    s.summary     = "RubyLuaBridge"
    s.description = "RubyLuaBridge allows one to embed a Lua virtual machine in a Ruby program and seamlessly interact with it."
    s.authors     = ["Evan Wies"]
    s.email       = "neomantra@gmail.com"
    s.files       = Dir.glob('lib/**/*.rb') + Dir.glob('ext/**/*.{cpp,h}')
    s.extensions  = ['ext/rubyluabridge/extconf.rb']
    s.homepage    = "https://bitbucket.org/neomantra/rubyluabridge/"
end

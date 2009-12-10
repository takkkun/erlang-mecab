require 'rake/clean'

NUM_OF_PROPS = 4

ERL_DIR, ERL_LIB_DIR, EI_DIR, DRIVER_DIR = lambda do
  props = `escript #{File.join %w(script props)}`
  lines = props.split $/
  fail props if lines.length < NUM_OF_PROPS

  lines.shift while lines.length > NUM_OF_PROPS
  lines[0] = "#{lines[0]}/usr"
  lines[3] = "priv/lib/#{lines[3]}"
  lines
end.call

BEAM_DIR = 'ebin'

def dirname(files)
  (files.map {|f| File.dirname f}.uniq - [BEAM_DIR]).sort_by {|f| f.length}
end

BEAM_OBJ = FileList['src/**/*.erl'].sub(/^src\//, "#{BEAM_DIR}/").ext 'beam'
BEAM_DIRS = dirname BEAM_OBJ

DRIVER_OBJ = "#{DRIVER_DIR}/mecab_drv.so"

CLOBBER.include [BEAM_OBJ, BEAM_DIRS, 'priv']

def resolve(ext)
  lambda {|obj| obj.sub(/^#{BEAM_DIR}\//, 'src/').sub(/\.[^.]+$/, ".#{ext}")}
end

ERLC = 'erlc'
ERLC_FLAGS = '-W'

CC = 'gcc'
CC_FLAGS = '-Wall -02'
SO_FLAGS = `uname`.chomp == 'Darwin' ?
           '-fno-common -bundle -undefined suppress -flat_namespace' :
           '-dynamiclib'

ERL_INCLUDE = "-I#{ERL_DIR}/include"
ERL_LIBS = "-L#{ERL_DIR}/lib -lerts"

EI_INCLUDE = "-I#{EI_DIR}/include"
EI_LIBS = "-L#{EI_DIR}/lib -lei -lerl_interface"

MECAB_INCLUDE = '`mecab-config --cflags`'
MECAB_LIBS = '`mecab-config --libs`'

(BEAM_DIRS + [DRIVER_DIR]).each {|dir| directory dir}

task :default => %w(beam driver)

desc 'Build beam files.'

file :beam => BEAM_DIRS + BEAM_OBJ

rule /\.beam$/ => resolve('erl') do |t|
  sh "#{ERLC} #{ERLC_FLAGS} -o #{File.dirname t.to_s} #{t.source}"
end

desc 'Build MeCab driver.'

task :driver => [DRIVER_DIR, DRIVER_OBJ]

file DRIVER_OBJ => 'src/mecab_drv.c' do |t|
  sh "#{CC} #{CC_FLAGS} #{SO_FLAGS} " +
     "#{ERL_INCLUDE} #{ERL_LIBS} " +
     "#{EI_INCLUDE} #{EI_LIBS} " +
     "#{MECAB_INCLUDE} #{MECAB_LIBS} " +
     "-o #{t} #{t.prerequisites}"
end

def install_dir
  version = `escript #{File.join %w(script version)}`
  fail version unless version =~ /^mecab: (.+)$/
  "#{ERL_LIB_DIR}/mecab-#{$1.chomp}"
end

desc 'Install to Erlang lib.'

task :install do
  dir = install_dir
  mkdir dir unless File.exists? dir
  cp_r BEAM_DIR, dir
  cp_r 'priv', dir
  cp_r 'src', dir
end

desc 'Uninstall from Erlang lib.'

task :uninstall do
  dir = install_dir
  rm_r dir if File.exists? dir
end

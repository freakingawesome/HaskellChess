# Pulled from http://pbrisbin.com/posts/automated_unit_testing_in_haskell/
# also needs:
#   apt-get install ruby1.9.1-dev
#   gem install guard-shell
#
# Runs the command and prints a notification
def execute(cmd)
  if system(cmd)
    n 'Build succeeded', 'hspec', :success
  else
    n 'Build failed', 'hspec', :failed
  end
end

def run_all_tests
  execute %{
    cabal test
  }
end

def run_tests(mod)
  specfile = "test/#{mod}Spec.hs"

  if File.exists?(specfile)
    files = [specfile]
  else
    files = Dir['test/**/*.hs']
  end

  execute "ghc -isrc -itest -e main #{files.join(' ')}"
end

guard :shell do
  watch(%r{.*\.cabal$})          { run_all_tests }
  watch(%r{test/SpecHelper.hs$}) { run_all_tests }
  watch(%r{src/(.+)\.hs$})       { run_all_tests }
  watch(%r{test/(.+)Spec\.hs$})  { run_all_tests }
  #watch(%r{src/(.+)\.hs$})       { |m| run_tests(m[1]) }
  #watch(%r{test/(.+)Spec\.hs$})  { |m| run_tests(m[1]) }
end

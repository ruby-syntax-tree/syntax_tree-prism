# frozen_string_literal: true

Gem::Specification.new do |spec|
  spec.name = "syntax_tree-yarp"
  spec.version = "0.1.0"
  spec.authors = ["Kevin Newton"]
  spec.email = ["kddnewton@gmail.com"]

  spec.summary = "WIP"
  spec.homepage = "https://github.com/ruby-syntax-tree/syntax_tree-yarp"
  spec.license = "MIT"
  spec.metadata = { "rubygems_mfa_required" => "true" }

  spec.files =
    Dir.chdir(__dir__) do
      `git ls-files -z`.split("\x0")
        .reject { |f| f.match(%r{^(test|spec|features)/}) }
    end

  spec.bindir = "exe"
  spec.executables = spec.files.grep(%r{^exe/}) { |f| File.basename(f) }
  spec.require_paths = %w[lib]

  spec.add_dependency "prettier_print", ">= 1.2.0"
  spec.add_dependency "yarp", ">= 0.12.0"
end

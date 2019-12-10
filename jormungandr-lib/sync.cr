#!/usr/bin/env nix-shell
#!nix-shell -p nix-prefetch-github crystal -i crystal

require "json"
require "http/client"

class JormungandrVersions
  EMPTY_HASH    = "0000000000000000000000000000000000000000000000000000"
  VERSIONS_FILE = "jormungandr-lib/versions.json"

  JSON.mapping(versions: Hash(String, Version))

  def self.update!
    from_json(File.read(VERSIONS_FILE)).update!
  end

  class Version
    JSON.mapping(version: String, sha256: String, cargoSha256: String)

    def initialize(@version, @sha256 : String = EMPTY_HASH, @cargoSha256 : String = EMPTY_HASH)
      @version = version_without_v
    end

    def calculate_sha256
      return unless sha256 == EMPTY_HASH
      puts "Calculating sha256 for #{version}"
      calculate "src" { |sum| @sha256 = sum; yield }
    end

    def calculate_cargoSha256
      return unless cargoSha256 == EMPTY_HASH
      puts "Calculating cargoSha256 for #{version}"
      calculate "cargoDeps" { |sum| @cargoSha256 = sum; yield }
    end

    def calculate(name : String)
      args = [
        "build", "-L", "-f", "./jormungandr-lib/calculate_hashes.nix", name,
        "--argstr", "version", version_without_v,
        "--argstr", "sha256", sha256.not_nil!,
        "--argstr", "cargoSha256", cargoSha256,
      ]

      puts "nix #{args.join(" ")}"

      Process.run("nix", args) do |process|
        process.error.gets_to_end.match(/^\s*got:\s*sha256:(.\S+)/m).try(&.[1]).try do |sum|
          yield sum
        end
      end
    end

    def version_without_v
      version.gsub(/^v/, "")
    end
  end

  def github_releases
    HTTP::Client.get("https://api.github.com/repos/input-output-hk/jormungandr/releases") do |resp|
      GithubReleases.from_json(resp.body_io)
    end
  end

  def update!
    github_releases.each do |release|
      version = JormungandrVersions::Version.new(release.tag_name)

      versions[release.nix_version]?.try do |existing_version|
        version.sha256 = existing_version.sha256
        version.cargoSha256 = existing_version.cargoSha256
      end

      version.calculate_sha256 do
        versions[release.nix_version] = version
        persist!
      end

      version.calculate_cargoSha256 do
        versions[release.nix_version] = version
        persist!
      end
    end
  end

  # TODO: use parallel execution
  def calculate_sha256_sums
    versions.values.each do |version|
      persist! if version.calculate_sha256
    end
  end

  # TODO: use parallel execution
  def calculate_cargoSha256_sums
    versions.values.each do |version|
      persist! if version.calculate_cargoSha256
    end
  end

  def persist!
    File.write(VERSIONS_FILE, self.to_pretty_json)
  end
end

alias GithubReleases = Array(GithubRelease)

class GithubRelease
  JSON.mapping(tag_name: String)

  def nix_version : String
    tag_name.gsub(/(\d+)\./) { "#{$1}_" }.gsub("+", "_").gsub(".", "_")
  end
end

JormungandrVersions.update!

puts "Updated all versions"
puts "Now you can update your network configuration with the correct version."
puts "Hit return when you want to continue, so I can fetch the new genesis (QA and Nightly only atm)"

gets

%w[qa nightly].each do |cluster|
  puts "Synchronizing with #{cluster}"
  system(%(scp testnet-deployer:jormungandr-#{cluster}/static/genesis.yaml jormungandr-lib/genesis-#{cluster}.yaml))
  puts "Gensis hash for #{cluster}:"
  system(%(ssh testnet-deployer "cd jormungandr-#{cluster}; direnv exec . jcli genesis hash < static/block-0.bin"))
end

puts "All done"
puts "please update the genesis hash in jormungandr-lib/default.nix manually, I'm not smart enough to do that yet."

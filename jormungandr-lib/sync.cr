#!/usr/bin/env nix-shell
#!nix-shell -I nixpkgs=./nix -p rsync crystal -i crystal

require "file_utils"
require "http/client"
require "json"

class JormungandrVersions
  EMPTY_HASH    = "0000000000000000000000000000000000000000000000000000"
  VERSIONS_FILE = "jormungandr-lib/versions.json"

  JSON.mapping(versions: Hash(String, Version))

  def self.update!
    from_json(File.read(VERSIONS_FILE)).update!
    puts "Updated all versions in #{VERSIONS_FILE}"
  end

  class Version
    JSON.mapping(version: String, rev: String, sha256: String, cargoSha256: String)

    def initialize(@version, @rev, @sha256 : String = EMPTY_HASH, @cargoSha256 : String = EMPTY_HASH)
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
        output = process.error.gets_to_end
        output.match(/^\s*got:\s*sha256:(.\S+)/m).try(&.[1]).try do |sum|
          yield sum
        end

        output.match(/^\s*got:\s*sha256-(.\S+)/m).try(&.[1]).try do |sum|
          yield convert_to_old(sum)
        end
      end
    end

    def convert_to_old(hash)
      output = IO::Memory.new
      Process.run("nix-hash", [ "--to-base32", "--type", "sha256", hash ], output: output, error: STDERR)
      output.to_s.strip
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
      version = JormungandrVersions::Version.new(release.tag_name, release.rev)

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

  def rev
    args = ["ls-remote", "https://github.com/input-output-hk/jormungandr", tag_name]
    puts "git #{args.join(" ")}"

    IO::Memory.new.tap { |memory|
      Process.run("git", args, output: memory, error: STDERR)
    }.to_s.split.first
  end
end

class JormungandrSynchronization
  NETWORKS = {
    "qa" => {
      server: "testnet-deployer",
      dir:    "jormungandr-qa",
    },
    "nightly" => {
      server: "testnet-deployer",
      dir:    "jormungandr-nightly",
    },
    "itn_rewards_v1" => {
      server: "mainnet-deployer",
      dir:    "jormungandr-incentivized",
    },
    "legacy" => {
      server: "testnet-deployer",
      dir:    "jormungandr-legacy",
    },
    "beta" => {
      server: "testnet-deployer",
      dir:    "jormungandr-beta",
    },
  }

  def self.sync!
    chan = Channel(Nil).new

    NETWORKS.each do |name, settings|
      puts "Synchronizing #{name} ..."
      spawn do
        new.sync(name, **settings)
        chan.send nil
      end
    end

    NETWORKS.size.times { chan.receive }
  end

  def sync(name, server, dir)
    unless sync_genesis_hash(name, server, dir)
      puts "#{name} is already synchronized"
      return
    end

    server_genesis = "#{server}:#{dir}/static/genesis.yaml"
    local_genesis = "jormungandr-lib/genesis-#{name}.yaml"
    bak = "#{local_genesis}.rsync.bak"

    if File.file?(local_genesis)
      FileUtils.cp local_genesis, bak
    end

    system(%(rsync -P #{server_genesis} #{bak}))

    if File.size(bak) / (1024.0 ** 2) > 1
      puts "truncating #{bak} genesis for because it's larger than 1 megabyte"

      File.open(bak, "r") do |in_io|
        content = Hash(String, JSON::Any).from_json(in_io)
        content["initial"] = JSON::Any.new(Array(JSON::Any).new)
        File.write(local_genesis, content.to_pretty_json)
      end
    else
      FileUtils.mv bak, local_genesis
    end
  end

  def sync_genesis_hash(name, server, dir)
    file = `nix eval --raw '((import ./. {}).jormungandrLib.environments.#{name}.genesisFile)'`.strip
    hash = `nix eval --raw '((import ./. {}).jormungandrLib.environments.#{name}.genesisHash)'`.strip

    cmd = "cd #{dir}; direnv exec . jcli genesis hash < static/block-0.bin"
    stdout, stderr = IO::Memory.new, IO::Memory.new
    process = Process.run("ssh", [server, cmd], error: stderr, output: stdout)

    unless process.success?
      puts "couldn't update hash for #{name}:"
      puts stderr.to_s
      return false
    end

    current_hash = stdout.to_s.strip

    if hash != current_hash
      old_default = File.read("jormungandr-lib/default.nix")
      File.write("jormungandr-lib/default.nix", old_default.gsub(hash, current_hash))
      return true
    end

    false
  end
end

JormungandrVersions.update!

puts "Now you can update your deployer with the correct version."
puts "Hit return to continue fetching the genesis from the deployers."

gets

JormungandrSynchronization.sync!

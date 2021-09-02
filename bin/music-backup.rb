#!/usr/bin/env ruby

backup_dir = '/Volumes/feynman/Music'

if !Dir.exists?(backup_dir)
  puts "Please create directory '#{backup_dir}'"
  exit 1
end

# -vr: verbose, recursive
# --size-only: don't copy files if dest and source are the same size
# --delete: delete files in dest that aren't in source
system("rsync -vr --size-only --delete #{Dir.home}/Music/iTunes/iTunes\\ Media/Music/ #{backup_dir}/")

puts 'Done'

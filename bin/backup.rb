#!/usr/bin/env ruby

def find_backup_drive
  %w(yudkowsky feynman Tyson SAGAN home-silver home-gold home-rose).each do |disk|
    if File.exists? "/Volumes/#{disk}"
      return disk
    end
  end

  puts 'Please insert your backup drive'
  exit 1
end

drive = find_backup_drive
backup_dir = "/Volumes/#{drive}/home"
puts "Backing up to #{backup_dir}..."

dirs = if drive.start_with?('home-')
         %w(books dotfiles Music)
       else
         %w(books code Compositions Documents dotfiles Movies Music Pictures thoughts)
       end

if !Dir.exists?(backup_dir)
  puts "Please create directory '#{backup_dir}'"
  exit 1
end

Dir.chdir(Dir.home) do
  dirs.each do |dir|
    if !Dir.exists?(dir)
      puts "~/#{dir} doesn't exist"
      next
    end

    puts '*' * 80
    puts "  backing up #{dir}..."
    puts '*' * 80

    # -vr: verbose, recursive
    # --size-only: don't copy files if dest and source are the same size
    # --delete: delete files in dest that aren't in source
    system("rsync -vr --size-only --delete #{Dir.home}/#{dir}/ #{backup_dir}/#{dir}")
  end

  system("cp ~/Library/Application\\ Support/Google/Chrome/Default/Bookmarks #{backup_dir}/bookmarks.json")
end

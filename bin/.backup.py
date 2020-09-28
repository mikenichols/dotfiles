#!/usr/bin/env python3

import os

def find_backup_drive():
    for disk in ['home-silver', 'home-gold', 'home-rose', 'SAGAN']:
        if os.path.exists(f'/Volumes/{disk}'):
            return disk
    print('Please insert your backup drive')
    exit(1)

drive = find_backup_drive()
backup_dir = f'/Volumes/{drive}/home'
print(f'Backing up to {backup_dir}...')

dirs = ['books', 'code', 'Compositions', 'Documents', 'dotfiles', 'Movies', 'Music', 'Pictures', 'thoughts']

if not os.path.exists(backup_dir):
    print(f"Please create directory '{backup_dir}'")
    exit(1)

home = os.path.expanduser('~')
os.chdir(home)

for dir in dirs:
    if not os.path.exists(dir):
        print(f"~/{dir} doesn't exist")
        continue

    print('*' * 80)
    print(f'  backing up {dir}...')
    print('*' * 80)

    # -vr: verbose, recursive
    # --size-only: don't copy files if dest and source are the same size
    # --delete: delete files in dest that aren't in source
    os.system(f'rsync -vr --size-only --delete {home}/{dir}/ {backup_dir}/{dir}')

print('\n  __________,\n /  Done!  /\n\'---------')

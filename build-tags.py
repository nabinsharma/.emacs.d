"""
Create TAGS (via ctags) files for major projects.
For more info, type "build-tags.py -h" in shell.

For each project directory, creates a single tag file
at the project's root folder.

Nabin Sharma
Nov 25, 2013.

"""

import argparse
import os
import subprocess
import sys


def generate_tags(dev_drive, ctags_dir):
  dev_drive = "%s:\\" % dev_drive
  ctags = "%s\\ctags.exe --recurse -e *" % ctags_dir
  print ctags
  for item in os.listdir(dev_drive):
    full_path = os.path.join(dev_drive, item)
    if (not os.path.isdir(os.path.join(full_path, '.svn')) and
        not os.path.isdir(os.path.join(full_path, '.git'))):
      print("Skipping %s: not in version control" % full_path)
      continue
    print("Building tags for %s " % full_path)
    os.chdir(full_path)
    subprocess.call(ctags)
  print "Done."


def main(argv=None):
  if argv is not None:
    sys.argv = argv
  parser = argparse.ArgumentParser(
    description=__doc__, formatter_class=argparse.RawTextHelpFormatter)
  parser.add_argument("--dev-drive", default="d",
                      help=("Disk where projects, for which tags are "
                            "being generated, sit."))
  parser.add_argument("--ctags-dir", default=r"c:\ctags58",
                      help=("Ctags directory (under which ctags.exe "
                            "can be found)."))
  args = parser.parse_args()
  generate_tags(args.dev_drive, args.ctags_dir)


if __name__ == "__main__":
  sys.exit(main())

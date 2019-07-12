###############################################################################
# NAME:             latex-template.sh
#
# AUTHOR:           Ethan D. Twardy <edtwardy@mtu.edu>
#
# DESCRIPTION:      This shell script sets up a basic latex environment in the
#                   current directory.
#
# CREATED:          05/15/2019
#
# LAST EDITED:      05/15/2019
###

# What it do:
# 1. Clone (or curl?) the latex-template repository
# 2. Run make to set up the pipenv
# 3. Create the basic latex file based on the first argument.
# 4. Populate the Makefile with the correct rules.
# 5. Create or update a .gitignore if there is a git repository in cwd.

# Goals: No .git repository, no temporary files, correct dates in all files

###############################################################################

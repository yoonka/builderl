#!/bin/sh

# This script initalizes a new project to use builderl as the build tool.
# It should be executed from an empty folder for the new project, e.g.:
# mkdir myproject_rel
# cd myproject_rel
# curl -s https://raw.githubusercontent.com/yoonka/builderl/master/priv/init_builderl.sh | sh

# Initialize git repository for the new project
git init
git submodule add https://github.com/yoonka/builderl.git deps/builderl

# Copy project skeleton from the cloned builderl submodule
cp -r -i deps/builderl/priv/skel/* .
cp -i deps/builderl/priv/_gitignore .gitignore

# Initialize builderl
ln -s ../deps/builderl bin/builderl
cp -i deps/builderl/makefiles/GNUmakefileBuilderl .

# Prefer gmake over make if installed
cmd=`(type gmake 2>/dev/null || type make 2>/dev/null) | tail -1 | awk '{ print $NF }'`
$cmd check-rebar
./bin/builderl.esh -uy

# Add all created files to the new repository
git add .git* GNU* rebar.config bin/* deps-versions/* etc/* lib/*

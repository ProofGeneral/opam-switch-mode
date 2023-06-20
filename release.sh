#!/usr/bin/env bash
# Author: Erik Martin-Dorel, 2023
# License for this script: MIT

set -eu

perl_replace() {
    local str1="$1"
    local str2="$2"
    perl -wpe 'BEGIN {$str1=shift @ARGV; $str2=shift @ARGV;}; s/$str1/$str2/g;' "$str1" "$str2"
}

do_release() {
    file=opam-switch-mode.el
    git checkout master
    line=$(grep "$file" -e Version:)
    version=${line/*Version: /}
    release=${version%-git}
    next=$(perl -wne 'my ($major,$minor) = $_ =~ m/^(.+)\.(.+)$/; print("$major.", $minor+1, "-git");' <<< "$release")
    line_release=$(perl_replace "$version" "$release" <<<"$line")
    line_next=$(perl_replace "$version" "$next" <<<"$line")
    cp -fv "$file" "$file~"

    perl_replace "$line" "$line_release" <"$file~" > "$file"
    git add -vu
    git commit -m "chore: Release $release"
    git tag -s "$release" -m "Version $release"

    perl_replace "$line" "$line_next" <"$file~" > "$file"
    git branch -D next || true
    git checkout -b next
    git add -vu
    git commit -m "chore: Prepare next dev cycle"
    git checkout master
    echo >&2 "Ready to: git push $(git remote) master $release"
    echo >&2 "Then, do: git merge next"
}
do_release

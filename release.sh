#!/usr/bin/env bash
# Author: Erik Martin-Dorel, 2023
# License for this script: MIT

set -eu

perl_replace() {
    local str1="$1"
    local str2="$2"
    local slurp="${3:+0777}"
    perl -wp"$slurp"e 'BEGIN{$str1=shift @ARGV; $str2=shift @ARGV; $s1=quotemeta($str1); }; s/$s1/$str2/g;' "$str1" "$str2"
}

tweak_changelog() {
    local file=NEWS.md
    local next="$1"
    local step="$2" # '1' or '2'
    local next_full
    next_full="## [$next] - $(date -I)"

    if [ "$step" = '1' ]; then
        local replace=(
'### Added

#'
'### Fixed

#'
'### Changed

#'
'### Removed

#')
        local r
        for r in "${replace[@]}"; do
            cp -f "$file" "$file~"
            perl_replace "$r" "#" "slurp" <"$file~" > "$file"
        done
        local line_unreleased
        line_unreleased=$(grep "$file" -e 'Unreleased.*\.\.\.HEAD')
        local released_tmp
        released_tmp=$(perl_replace 'Unreleased' "$next" <<< "$line_unreleased")
        local released
        released=$(perl_replace 'HEAD' "$next" <<< "$released_tmp")
        cp -fv "$file" "$file~"
        perl_replace "## [Unreleased]" "$next_full" <"$file~" > "$file"
        cp -fv "$file" "$file~"
        perl_replace "$line_unreleased" "$released" <"$file~" > "$file"

    elif [ "$step" = '2' ]; then
        local section='## [Unreleased]

### Added

### Fixed

### Changed

### Removed

'
        local unreleased="[Unreleased]: https://github.com/ProofGeneral/opam-switch-mode/compare/$next...HEAD"
        cp -fv "$file" "$file~"
        perl_replace "$next_full" "$section$next_full" <"$file~" > "$file"
        cp -fv "$file" "$file~"
        perl_replace '<!-- bottom -->' "<!-- bottom -->
$unreleased" <"$file~" > "$file"

    else
        echo "tweak_changelog: wrong value for \$2" >&2
        exit 1
    fi
}

do_release() {
    local file=opam-switch-mode.el
    git checkout master
    local line
    line=$(grep "$file" -e Version:)
    local version=${line/*Version: /}
    local release=${version%-git}
    local next
    next=$(perl -wne 'my ($major,$minor) = $_ =~ m/^(.+)\.(.+)$/; print("$major.", $minor+1, "-git");' <<< "$release")
    local line_release
    line_release=$(perl_replace "$version" "$release" <<<"$line")
    local line_next
    line_next=$(perl_replace "$version" "$next" <<<"$line")
    cp -fv "$file" "$file~"

    perl_replace "$line" "$line_release" <"$file~" > "$file"
    tweak_changelog "$release" 1
    git add -vu
    git commit -m "chore: Release $release"
    git tag -s "$release" -m "Version $release"

    perl_replace "$line" "$line_next" <"$file~" > "$file"
    tweak_changelog "$release" 2
    git branch -D next || true
    git checkout -b next
    git add -vu
    git commit -m "chore: Prepare next dev cycle"
    git checkout master
    echo >&2 "Ready to: git push $(git remote) master $release"
    echo >&2 "Then, do: git merge next"
}

do_release

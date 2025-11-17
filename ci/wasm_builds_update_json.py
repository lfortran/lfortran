import datetime
from json import load, dump
import os
import shutil
import sys

dest_dir = sys.argv[1]
version = sys.argv[2]
lfortran_commit_sha = sys.argv[3]

# Retention policy
MAX_DEV_BUILDS = 10  # Keep only the latest 10 debug builds
# Release builds have no limit (keep all)

filename = "data.json"
if os.path.exists(filename):
    d = load(open(filename))
else:
    d = {"dev": [], "release": []}

# Add new entry
entry = {
    "url": "https://lfortran.github.io/wasm_builds/%s/%s" % \
            (dest_dir, lfortran_commit_sha),
    "version": version,
    "lfortran_commit_sha": lfortran_commit_sha,
    "created": str(datetime.datetime.now())
}
d[dest_dir].insert(0, entry)

# Apply retention policy for dev builds
if dest_dir == "dev" and len(d[dest_dir]) > MAX_DEV_BUILDS:
    print(f"Applying retention policy: keeping only {MAX_DEV_BUILDS} latest dev builds")
    
    # Get builds to remove (everything after the first MAX_DEV_BUILDS entries)
    builds_to_remove = d[dest_dir][MAX_DEV_BUILDS:]
    
    # Remove old build directories
    for build in builds_to_remove:
        build_sha = build["lfortran_commit_sha"]
        build_path = os.path.join(dest_dir, build_sha)
        if os.path.exists(build_path):
            print(f"Removing old build directory: {build_path}")
            shutil.rmtree(build_path)
        else:
            print(f"Build directory not found (already removed?): {build_path}")
    
    # Keep only the latest MAX_DEV_BUILDS entries in JSON
    d[dest_dir] = d[dest_dir][:MAX_DEV_BUILDS]
    print(f"Kept {len(d[dest_dir])} dev builds in data.json")
elif dest_dir == "release":
    print(f"Release build: no retention limit (keeping all {len(d[dest_dir])} builds)")

print("Saving to %s." % filename)
with open(filename, "w") as f:
    dump(d, f, indent=4, ensure_ascii=False, sort_keys=True)

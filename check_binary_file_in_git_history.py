import subprocess

# Open file_path, Read it with mode 'r' which expects a utf-8 encode.
# If the file is binary it'd conflict with
# text mode 'r', leading to raised error `UnicodeDecodeError`.
def is_file_binary(file_path: str) -> bool:
    try:
        with open(file_path, 'r') as fp:
            fp.read(16)
            return False
    except UnicodeDecodeError:
            return True


# Get all commit hashes difference between `origin/main` and current `HEAD`
def get_commit_hash_differences() -> list :
    # Run command `git rev-list origin/main..HEAD`
    # Use `subporcess.PIPE` as the output buffer to fetch the list of commit hashes
    completedProcess_object =  subprocess.run(["git" ,"rev-list" ,"origin/main..HEAD"],
                                                stdout=subprocess.PIPE
                                                , text=True)
    # Create list out of the resulting commit hases. 
    commit_hashes_listed  : list  = completedProcess_object.stdout.split("\n")
    if(commit_hashes_listed[-1] == ""):
        commit_hashes_listed.remove("") # Remove trailing empty string.
    print("number of commits from current PR branch against origin/main is :", len(commit_hashes_listed))
    print (completedProcess_object.stdout) # print the whole string of commit hashes.
    return commit_hashes_listed

# Fetch all files pathes that has beend changed from current `HEAD` against `origin/main`.
def get_changed_files() -> list :
    # Run command `git diff --name-only origin/main --diff-filter=d`
    # Notice we filter out the deleted files.
    completedProcess_object = subprocess.run(["git","diff","--name-only","origin/main", "--diff-filter=d"],
                                            stdout=subprocess.PIPE, text=True)
    file_paths : list = completedProcess_object.stdout.split("\n")
    if(file_paths[-1] == ""): file_paths.remove("") # Remove trailing empty string.
    return file_paths




def run_test() :
    commit_hashes_list = get_commit_hash_differences()
    # Run git `checkout` to each commit_hash between `origin/main` --> `HEAD`
    for commit_hash in commit_hashes_list:
        subprocess.run(["git","checkout",commit_hash], 
                       stdout=subprocess.PIPE,stderr=subprocess.PIPE,text=True) 
        print("checked out to commit --> ",commit_hash)

        file_paths : list = get_changed_files() # fetch changed files from current-checkout-to commit.
        for file_path in file_paths:
            if (is_file_binary(file_path)):
                print("FAIL --- binary file detected")
                print(f"Binary File Name : ({file_path}) In Commit : ({commit_hash})")
                raise SystemExit(1)

        print("OK --- no binary file found")
        print("\n--------------------------------------------------\n")


    
    print("CLEAN : No binary file found in any commit in this PR")
    print("\n--------------------------------------------------\n")
    print("==> Checking out to latest commit in the PR branch")
    print("==> Commit hash :", commit_hashes_list[0])
    subprocess.run(["git","checkout",commit_hashes_list[0]], 
            stdout=subprocess.PIPE, stderr=subprocess.PIPE,text=True)
    print("\n--------------------------------------------------\n")
    print("DONE")


run_test()


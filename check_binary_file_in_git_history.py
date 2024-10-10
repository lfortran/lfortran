import subprocess

def is_file_binary(path: str) -> bool:
    try:
        with open(path, 'r') as fp:
            fp.read(16)
            return False
    except UnicodeDecodeError:
            return True
    
def get_commit_hashes_PR_against_main() -> list : # returns list of strings
    completedProcess_object =  subprocess.run(["git" ,"rev-list" ,"origin/main..HEAD"], stdout=subprocess.PIPE, text=True)
    commit_hashes_listed  : list  = completedProcess_object.stdout.split("\n")
    if(commit_hashes_listed[-1] == ""): commit_hashes_listed.remove("")
    print("number of commit from current PR branch against origin/main is :", len(commit_hashes_listed))
    print (completedProcess_object.stdout)
    return commit_hashes_listed

def get_changed_files() -> list : # returns list of strings.
    completedProcess_object = subprocess.run(["git","diff","--name-only","origin/main", "--diff-filter=d"], stdout=subprocess.PIPE, text=True)
    file_paths : list = completedProcess_object.stdout.split("\n")
    if(file_paths[-1] == "" ): file_paths.remove("")
    return file_paths



def run() :
    commit_hashes_list = get_commit_hashes_PR_against_main()

    for commit_hash in commit_hashes_list:
        subprocess.run(["git","checkout",commit_hash], stdout=subprocess.PIPE,stderr=subprocess.PIPE,text=True)
        print("check out to commit-->",commit_hash)
        file_paths : list = get_changed_files()
        for path in file_paths:
            if (is_file_binary(path)):
                raise SystemExit(f"ERROR : FOUND PUSHED BINARY FILE --> ({path}) IN COMMIT ({commit_hash})")
        print("--------------------------------------------------")

    print("CLEAN : No binary file found")
    print("return back to latest commit in branch")
    subprocess.run(["git","checkout",commit_hashes_list[0]], stdout=subprocess.PIPE,stderr=subprocess.PIPE,text=True)
    print("DONE")



run()


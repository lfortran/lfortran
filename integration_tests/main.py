import os


def list_lp_files():
    return [f for f in os.listdir() if os.path.isfile(f) and f.startswith("lp_")]


def rename_files(files):
    for old_name in files:
        new_name = old_name[3:]  # Remove first 3 characters: "lp_"
        os.rename(old_name, new_name)
        print(f"Renamed: {old_name} → {new_name}")


def main():
    lp_files = list_lp_files()

    if not lp_files:
        print("No files with prefix 'lp_' found.")
        return

    print("Found files:")
    for f in lp_files:
        print(f"  {f}")

    confirm = (
        input("\nDo you want to rename all of these files by removing 'lp_'? (y/n): ")
        .strip()
        .lower()
    )
    if confirm == "y":
        rename_files(lp_files)
        print("\nRenaming complete.")
    else:
        print("\nOperation cancelled.")


if __name__ == "__main__":
    main()
